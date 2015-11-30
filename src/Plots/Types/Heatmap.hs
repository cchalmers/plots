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

import           Control.Monad.State
import qualified Data.Foldable                   as F
import           Data.Typeable
import           Data.Vector.Unboxed             ((!))
import qualified Data.Vector.Unboxed             as U

import           Diagrams.Coordinates.Isomorphic
import           Diagrams.Prelude

import           Plots.Axis
import           Plots.Style
import           Plots.Types
import           Plots.Utils

-- data HeatMap n = HeatMap
--   { hmFunction :: Int -> Int -> Double
--   , hmExtent   :: V2 Int  -- total x and y boxes
--   , hmStart    :: P2 n
--   , hmSize     :: V2 n    -- width and height of each box
--   } deriving Typeable

-- I'm still unsure of the "right" way to do this. On the one hand, by
-- restricting it to int users can read off data from an array without
-- any problems. On the other hand it's not an ideal representation for
-- heat maps of functions. Maybe we need two primitives.

-- | Squares for heat map. Since some renderers leave gaps for adjacent
--   blocks, squares are overlapped to avoid this.
-- heatSquares
--   :: (TypeableFloat n, Renderable (Path V2 n) b)
--   => n
--   -> ColourRange
--   -> QDiagram b V2 n Any
-- heatSquares (V2 x y) f =
--   alaf Dual F.foldMap mk ps
--     # lwO 0
-- -- foldMap is reversed using Dual so the squares are placed in the correct order
--   where
--     ps = [ V2 i j | i <- [0..x-1], j <- [0..y-1] ]
--     mk v@(V2 i j) =
--       rect w h
--         # alignBL
--         # translate (fromIntegral <$> v)
--         # fc (f v)
--         where
--           -- Squares that are not on the top left edge are slightly
--           -- bigger to remove phantom gaps
--           w | i == x    = 1
--             | otherwise = 1.5
--           h | j == y    = 1
--             | otherwise = 1.5

------------------------------------------------------------------------
-- Heatmap Lenses
------------------------------------------------------------------------

-- class HasHeatmap a v n | a -> v n where
--   heat :: Lens' a (HeatPlot v n)

--   drawGrid :: Lens' a Bool
--   drawGrid = heatmp . lens hmGrid (\s b -> (s {hGrid = b}))

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

-- | Render the heat map as an embedded image.
-- imageHeatRender
--   :: Renderable (Image n) b
--   => HeatMatrix
--   -> ColourMap
--   -> QDiagram b v n Any
-- imageHeatRender = undefined

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

-- -- | Render the heat map as an embedded image.
-- pathHeatRender'
--   :: Renderable (Path V2 n) b
--   => HeatMatrix
--   -> ColourMap
--   -> QDiagram b v n Any
-- pathHeatRender' (HeatMatrix (V2 x y) f) = alaf Dual F.foldMap mk ps # lwO 0
-- pathHeatRender' hm cm = alaf Dual F.ifoldMapOf heatMatrixPoints mk ps # lwO 0
--   where
--     mk v@(V2 x y) a =
--       rect w h
--         # alignBL
--         # translate (fromIntegral <$> v)
--         # fcA (cm ^. ixColour (toRational a))

    -- where

-- | Squares for heat map. Since some renderers leave gaps for adjacent
--   blocks, squares are overlapped to avoid this.
-- heatSquares
--   :: (TypeableFloat n, Renderable (Path V2 n) b)
--   => V2 n
--   -> ColourRange
--   -> QDiagram b V2 n Any
-- heatSquares (V2 x y) f = alaf Dual F.foldMap mk ps # lwO 0
-- -- foldMap is reversed using Dual so the squares are placed in the correct order
--   where
--     ps = [ V2 i j | i <- [0..x-1], j <- [0..y-1] ]
--     mk v@(V2 i j) =
--       rect w h
--         # alignBL
--         # translate (fromIntegral <$> v)
--         # fc (f v)
--         where
--           -- Squares that are not on the top left edge are slightly
--           -- bigger to remove phantom gaps
--           w | i == x    = 1
--             | otherwise = 1.5
--           h | j == y    = 1
--             | otherwise = 1.5

-- ------------------------------------------------------------------------
-- -- Heat matrix
-- ------------------------------------------------------------------------

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
class HasHeatmap f a b | a -> b where
  -- | Lens onto the heatmap options.
  heatMapOptions :: LensLike' f a (HeatMap b (N a))

  -- | Whether there should be grid lines draw for the heat map. Default
  --   is 'False'.
  heatMapGridVisible :: Functor f => LensLike' f a Bool
  heatMapGridVisible = heatMapOptions . lens hGridVisible (\s b -> (s {hGridVisible = b}))

  -- | The style applied to the grid lines for the heat map, if they're
  --   visible.
  heatMapGridStyle :: Functor f => LensLike' f a (Style V2 (N a))
  heatMapGridStyle = heatMapOptions . lens hGridSty (\s b -> (s {hGridSty = b}))

  -- | The size of each individual square in the heat map.
  heatMapSize :: Functor f => LensLike' f a (V2 (N a))
  heatMapSize = heatMapOptions . lens hSize (\s b -> (s {hSize = b}))

  -- | The starting point for the heat map.
  heatMapStart :: Functor f => LensLike' f a (P2 (N a))
  heatMapStart = heatMapOptions . lens hStart (\s b -> (s {hStart = b}))

  -- | Limits @(a,b)@ used on the data such that @a@ is the start of the
  --   'ColourMap' and @b@ is the end of the 'ColourMap'. Default is @(0,1)@.
  heatMapLimits :: Functor f => LensLike' f a (Maybe (Double, Double))
  heatMapLimits = heatMapOptions . lens hLimits (\s b -> (s {hLimits = b}))

  -- | Funtion used to render the heat map.
  heatMapRender :: Functor f => LensLike' f a (HeatMatrix -> ColourMap -> QDiagram b V2 (N a) Any)
  heatMapRender = heatMapOptions . lens hDraw (\s b -> (s {hDraw = b}))

instance OrderedField n => Enveloped (HeatMap b n) where
  getEnvelope HeatMap {..} = getEnvelope (fromCorners hStart (hStart .+^ hSize))

instance (Typeable b, TypeableFloat n, Renderable (Path V2 n) b)
    => Plotable (HeatMap b n) b where
  renderPlotable s _sty HeatMap {..} =
      grid <> hDraw matrix' (s^.specColourMap)
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
  { hMatrix      = mat
  , hStart       = origin
  , hSize        = fmap fromIntegral $ hmSize mat
  , hGridSty     = mempty
  , hGridVisible = False
  , hLimits      = Nothing
  , hDraw        = pathHeatRender
  }

-- | Add a 'HeatMap' plot.
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

