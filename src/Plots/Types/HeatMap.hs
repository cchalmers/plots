{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Plots.Types.HeatMap
-- Copyright   :  (C) 2016 Christopher Chalmers
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Christopher Chalmers
-- Stability   :  experimental
-- Portability :  non-portable

-- A heat map is a graphical representation of data where the individual
-- values contained in a matrix are represented as colours.
--
----------------------------------------------------------------------------


module Plots.Types.HeatMap
  ( HeatMap
  , heatMap
  , heatMap'

  -- * Lenses
  , HasHeatMap (..)

  -- * Heat matrix
  , HeatMatrix
  , heatImage
  , hmPoints
  , hmSize
  , normaliseHeatMatrix

  -- ** Rendering functions
  , pixelHeatRender
  , pathHeatRender

  -- * Low level construction
  , mkHeatMap
  , mkHeatMatrix
  , mkHeatMatrix'

  ) where

import           Control.Lens                    hiding (transform, ( # ))

import           Control.Monad.State
import qualified Data.Foldable                   as F
import           Data.Typeable
import           Data.Vector.Unboxed             ((!))
import qualified Data.Vector.Unboxed             as U

import           Codec.Picture
import           Diagrams.Coordinates.Isomorphic
import           Diagrams.Prelude

import           Plots.Axis
import           Plots.Style
import           Plots.Types
import           Plots.Util

------------------------------------------------------------------------
-- Heatmap
------------------------------------------------------------------------

-- | 2D Array of 'Double's.
data HeatMatrix = HeatMatrix
  { hmSize :: !(V2 Int)
  , hmFun  :: V2 Int -> Double
  }

-- | Create a heat matrix from an extent and a function to extract the
--   values.
mkHeatMatrix :: V2 Int -> (V2 Int -> Double) -> HeatMatrix
mkHeatMatrix = HeatMatrix

-- | Create a heat matrix from a list of lists of values. All sublists
--   should have the same length
mkHeatMatrix' :: Foldable f => f (f Double) -> HeatMatrix
mkHeatMatrix' xss =
  case F.length <$> preview folded xss of
    Just y  -> HeatMatrix (V2 x y) f
    Nothing -> HeatMatrix zero (const 0)
  where
    x           = F.length xss
    v           = U.fromList $ toListOf (folded . folded) xss
    f (V2 i j)  = v ! (x*j + i)

-- | Normalise a heat matrix so all values lie in the (0,1) range. Also
--   returns the lower and upper limit of the original heat matrix.
normaliseHeatMatrix :: HeatMatrix -> ((Double, Double), HeatMatrix)
normaliseHeatMatrix hm = (range, hm { hmFun = (/ (b - a)) . (+a) . hmFun hm })
  where range@(a,b) = minMaxOf hmPoints hm
        -- XXX what if range = 0?

-- | Indexed traversal over the values of a 'HeatMatrix'.
hmPoints :: IndexedTraversal' (V2 Int) HeatMatrix Double
hmPoints f (HeatMatrix e@(V2 x y) ixF) = go 0 0 <&> \vs ->
  let v             = U.fromListN (x*y) vs
      ixF' (V2 i j) = U.unsafeIndex v (x*j + i)
  in  HeatMatrix e ixF'
  where
    -- V2 x y = hmExtent
    go !i !j
      | i >= x    = go 0 (j+1)
      | j >= y    = pure []
      | otherwise = (:) <$> indexed f (V2 i j) (ixF $ V2 i j) <*> go (i+1) j
{-# INLINE hmPoints #-}

-- Rendering heat matrices --------------------------------------------

-- | Render an heatmap as an 'ImageRGB8' with @n@ pixels per heat matrix
--   point.
pixelHeatRender
  :: (Renderable (DImage n Embedded) b, TypeableFloat n)
  => Int
  -> HeatMatrix
  -> ColourMap
  -> QDiagram b V2 n Any
pixelHeatRender n hm cm =
  scale (1/fromIntegral n) . alignBL . image $ DImage (ImageRaster (ImageRGB8 img)) (x*n) (y*n) mempty
  where
    img    = heatImage n hm cm
    V2 x y = hmSize hm

heatImage :: Int -> HeatMatrix -> ColourMap -> Image PixelRGB8
heatImage n hm cm = generateImage (\i j -> getPx (V2 (i `div` n) ((y - j - 1) `div` n))) x y
  where
    V2 x y  = hmSize hm * V2 n n
    getPx v = colourToPixel . fromAlphaColour $ cm ^. ixColour (realToFrac $ hmFun hm v)

colourToPixel :: Colour Double -> PixelRGB8
colourToPixel c = PixelRGB8 r g b
  where RGB r g b = toSRGB24 c

-- | Render the heat map as squares.
pathHeatRender
  :: (Renderable (Path V2 n) b, TypeableFloat n)
  => HeatMatrix
  -> ColourMap
  -> QDiagram b V2 n Any
pathHeatRender hm cm = ifoldMapOf hmPoints mk hm # lwO 0
  where
    mk v@(V2 i j) a =
      rect w h
        # alignTR
        # translate (fromIntegral <$> v ^+^ 1)
        # fcA (cm ^. ixColour (toRational a))
      where
        -- Squares that are not on the top left edge are slightly
        -- bigger to remove phantom gaps
        w | i == 0    = 1
          | otherwise = 1.5
        h | j == 0    = 1
          | otherwise = 1.5

------------------------------------------------------------------------
-- Heat matrix
------------------------------------------------------------------------

-- | A mapping from points in a 2D axis do 'Double's. These 'Double's
--   are converted to colours using the axis 'ColourMap'.
data HeatMap b n = HeatMap
  { hMatrix      :: HeatMatrix
  , hStart       :: P2 n
  , hSize        :: V2 n
  , hGridSty     :: Style V2 n
  , hGridVisible :: Bool
  , hLimits      :: Maybe (Double,Double)
  , hDraw        :: HeatMatrix -> ColourMap -> QDiagram b V2 n Any
  } deriving Typeable

type instance V (HeatMap b n) = V2
type instance N (HeatMap b n) = n

-- | Class of things that let you change the heatmap options.
class HasHeatMap f a b | a -> b where
  -- | Lens onto the heatmap options.
  heatMapOptions :: LensLike' f a (HeatMap b (N a))

  -- | Whether there should be grid lines draw for the heat map.
  --
  --   Default is 'False'.
  heatMapGridVisible :: Functor f => LensLike' f a Bool
  heatMapGridVisible = heatMapOptions . lens hGridVisible (\s b -> (s {hGridVisible = b}))

  -- | The style applied to the grid lines for the heat map, if they're
  --   visible.
  --
  --   Default is 'mempty'.
  heatMapGridStyle :: Functor f => LensLike' f a (Style V2 (N a))
  heatMapGridStyle = heatMapOptions . lens hGridSty (\s b -> (s {hGridSty = b}))

  -- | The size of each individual square in the heat map.
  --
  --   Default is @'V2' 1 1@.
  heatMapSize :: Functor f => LensLike' f a (V2 (N a))
  heatMapSize = heatMapOptions . lens hSize (\s b -> (s {hSize = b}))

  -- | The size of the full extend of the heat map.
  --
  --   Default is extent of the heat matrix.
  heatMapExtent :: (Functor f, Fractional (N a)) => LensLike' f a (V2 (N a))
  heatMapExtent = heatMapOptions . l where
    l f hm = f (hSize hm * s) <&> \x -> hm { hSize = x / s }
      where s = fmap fromIntegral (hmSize $ hMatrix hm)

  -- | The starting point at the bottom left corner of the heat map.
  --
  --   Default is 'origin'
  heatMapStart :: Functor f => LensLike' f a (P2 (N a))
  heatMapStart = heatMapOptions . lens hStart (\s b -> (s {hStart = b}))

  -- | The center point of the heat map.
  heatMapCentre :: (Functor f, Fractional (N a)) => LensLike' f a (P2 (N a))
  heatMapCentre = heatMapOptions . l where
    l f hm = f (hStart hm .+^ v) <&> \p -> hm { hStart = p .-^ v }
      where v = fmap fromIntegral (hmSize $ hMatrix hm) * hSize hm / 2

  -- | Limits @(a,b)@ used on the data such that @a@ is the start of the
  --   'ColourMap' and @b@ is the end of the 'ColourMap'. Default is @(0,1)@.
  heatMapLimits :: Functor f => LensLike' f a (Maybe (Double, Double))
  heatMapLimits = heatMapOptions . lens hLimits (\s b -> (s {hLimits = b}))

  -- | Funtion used to render the heat map. See 'pathHeatRender' and
  --   'pixelHeatRender'.
  --
  --   Default is 'pathHeatRender'.
  heatMapRender :: Functor f => LensLike' f a (HeatMatrix -> ColourMap -> QDiagram b V2 (N a) Any)
  heatMapRender = heatMapOptions . lens hDraw (\s b -> (s {hDraw = b}))

instance HasHeatMap f (HeatMap b n) b where
  heatMapOptions = id

instance (Functor f, HasHeatMap f a b) => HasHeatMap f (Plot a b) b where
  heatMapOptions = rawPlot . heatMapOptions

instance OrderedField n => Enveloped (HeatMap b n) where
  getEnvelope hm = getEnvelope (fromCorners p (p .+^ v))
    where p = view heatMapStart hm
          v = view heatMapExtent hm

instance (Typeable b, TypeableFloat n, Renderable (Path V2 n) b)
    => Plotable (HeatMap b n) b where
  renderPlotable s _sty HeatMap {..} =
      transform (s^.specTrans) $
        grid <> hDraw matrix' (s^.specColourMap)
                  # transform (scaleV hSize)
                  # moveTo hStart
    where
      --- TODO
      grid = mempty

      --- XXX need to give _range to the axis somehow (for colour bar range)
      (_range, matrix') = normaliseHeatMatrix hMatrix

  -- XXX make better
  defLegendPic sty HeatMap {..} = square 5 # applyAreaStyle sty

scaleV :: (Additive v, Fractional n) => v n -> Transformation v n
scaleV v = fromLinear f f
  where f = (liftU2 (*) v) <-> (\u -> liftU2 (/) u v)

-- | Construct a 'Heatmap' using the given 'HeatMatrix'.
mkHeatMap :: (Renderable (Path V2 n) b, TypeableFloat n)
          => HeatMatrix -> HeatMap b n
mkHeatMap mat = HeatMap
  { hMatrix      = mat
  , hStart       = origin
  , hSize        = V2 1 1 -- fmap fromIntegral $ hmSize mat
  , hGridSty     = mempty
  , hGridVisible = False
  , hLimits      = Nothing
  , hDraw        = pathHeatRender
  }

-- | Add a 'HeatMap' plot using the extent of the heatmap and a
--   generating function.
--
-- @
-- 'heatMap' :: 'V2' 'Int'     -> ('V2' 'Int' -> 'Double')     -> 'State' ('Plot' ('HeatMap' b n)) () -> 'State' ('Axis' b 'V2' n) ()
-- 'heatMap' :: ('Int', 'Int') -> (('Int', 'Int') -> 'Double') -> 'State' ('Plot' ('HeatMap' b n)) () -> 'State' ('Axis' b 'V2' n) ()
-- @
heatMap
  :: (VectorLike V2 Int i,
      TypeableFloat n,
      Typeable b,
      MonadState (Axis b V2 n) m,
      Renderable (Path V2 n) b)
  => i             -- ^ extent of array
  -> (i -> Double) -- ^ heat from index
  -> State (Plot (HeatMap b n) b) ()
                   -- ^ changes to plot options
  -> m ()          -- ^ add plot to 'Axis'
heatMap i f s = do
  let hm    = mkHeatMatrix (view unvectorLike i) (f . view vectorLike)
      (r,_) = normaliseHeatMatrix hm
  addPlotable (mkHeatMap hm) s
  -- this is a pretty inefficient way to do this
  colourBarRange .= over both realToFrac r

-- | Add a 'HeatMap' plot using the extent of the heatmap and a
-- generating function without changes to the heap map options.
--
-- @
-- 'heatMap' :: 'V2' 'Int'     -> ('V2' 'Int' -> 'Double')     -> 'State' ('Axis' b 'V2' n) ()
-- 'heatMap' :: ('Int', 'Int') -> (('Int', 'Int') -> 'Double') -> 'State' ('Axis' b 'V2' n) ()
-- @
heatMap'
  :: (VectorLike V2 Int i,
      TypeableFloat n,
      Typeable b,
      MonadState (Axis b V2 n) m,
      Renderable (Path V2 n) b)
  => i             -- ^ extent of array
  -> (i -> Double) -- ^ heat from index
  -> m ()          -- ^ add plot to 'Axis'
heatMap' i f = heatMap i f (return ())

