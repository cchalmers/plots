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

module Plots.Types.HeatMap where
  -- ( HeatMap
  -- , simpleHeatMap
  --   -- * Prism
  -- , _HeatMap


  --   -- * Lenses
  -- , HeatMapBars
  -- , HeatMapWidth
  -- , HeatMapSpacing
  -- , HeatMapIsVerticle
  -- ) where

import           Control.Lens                    hiding (transform, ( # ))
import           Language.Haskell.TH.Syntax      (mkName)

import           Control.Monad.State
import qualified Data.Foldable                   as F
import           Data.Typeable
import           Data.Vector.Unboxed             ((!))
import qualified Data.Vector.Unboxed             as U

import           Diagrams.Coordinates.Isomorphic
import           Diagrams.Prelude
-- import           Diagrams.TwoD.Image
import Codec.Picture

import           Plots.Axis
import           Plots.Style
import           Plots.Types
import           Plots.Utils

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

-- | Render an heatmap as an image.
pixelHeatRender
  :: (Renderable (DImage n Embedded) b, TypeableFloat n)
  => HeatMatrix
  -> ColourMap
  -> QDiagram b V2 n Any
pixelHeatRender hm cm =
  scale (1/4) . alignBL . image $ DImage (ImageRaster (ImageRGB8 img)) (x*4) (y*4) mempty
  where
    img    = heatImage hm cm
    V2 x y = hmSize hm

heatImage :: HeatMatrix -> ColourMap -> Image PixelRGB8
heatImage hm cm = generateImage (\i j -> getPx (V2 (i `div` 4) (j `div` 4))) x y
  where
    V2 x y  = hmSize hm * 4
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
    V2 x y = hmSize hm
    mk v@(V2 i j) a =
      rect w h
        # alignBL
        # translate (fromIntegral <$> v)
        # fcA (cm ^. ixColour (toRational a))
      where
        -- Squares that are not on the top left edge are slightly
        -- bigger to remove phantom gaps
        w | i == x    = 1
          | otherwise = 1.5
        h | j == y    = 1
          | otherwise = 1.5

------------------------------------------------------------------------
-- Heat matrix
------------------------------------------------------------------------

data HeatMap b n = HeatMap
  { hMatrix             :: HeatMatrix
  , _heatMapStart       :: P2 n
  , _heatMapSize        :: V2 n
  , _heatMapGridStyle   :: Style V2 n
  , _heatMapGridVisible :: Bool
  , _heatMapGridLimits  :: Maybe (Double,Double)
  , _heatMapRender      :: HeatMatrix -> ColourMap -> QDiagram b V2 n Any
  } deriving Typeable

makeLensesWith (classyRules & lensClass . mapped . _Just . _2 .~ mkName "heatMapOptions") ''HeatMap

type instance V (HeatMap b n) = V2
type instance N (HeatMap b n) = n
type instance BackendType (HeatMap b n) = b

instance (HasHeatMap p b n, b ~ BackendType p, n ~ N p) => HasHeatMap (Plot p b) b n where
  heatMapOptions = rawPlot . heatMapOptions

instance OrderedField n => Enveloped (HeatMap b n) where
  getEnvelope HeatMap {..} = getEnvelope (fromCorners _heatMapStart (_heatMapStart .+^ _heatMapSize))

instance (Typeable b, TypeableFloat n, Renderable (Path V2 n) b)
    => Plotable (HeatMap b n) b where
  renderPlotable s _sty HeatMap {..} =
      transform (s^.specTrans) $
        grid <> _heatMapRender matrix' (s^.specColourMap)
    where
      --- TODO
      grid = mempty

      --- XXX need to give _range to the axis somehow (for colour bar range)
      (_range, matrix') = normaliseHeatMatrix hMatrix

  -- XXX make better
  defLegendPic sty HeatMap {..} = square 5 # applyAreaStyle sty

-- | Construct a 'Heatmap' using the given 'HeatMatrix'.
mkHeatMap :: (Renderable (Path V2 n) b, TypeableFloat n)
          => HeatMatrix -> HeatMap b n
mkHeatMap mat = HeatMap
  { hMatrix             = mat
  , _heatMapStart       = origin
  , _heatMapSize        = fmap fromIntegral $ hmSize mat
  , _heatMapGridStyle   = mempty
  , _heatMapGridVisible = False
  , _heatMapGridLimits  = Nothing
  , _heatMapRender      = pathHeatRender
  }

-- | Add a 'HeatMap' plot using the extent of the heatmap and a generating function.
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
heatMap i f = addPlotable (mkHeatMap hm)
  where
  hm = mkHeatMatrix (view unvectorLike i) (f . view vectorLike)

