{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE RecordWildCards  #-}

module Plots.Types.Heatmap where
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

import           Data.Typeable
import qualified Data.Vector.Unboxed as U
import           Data.Vector.Unboxed ((!))
import qualified Data.Foldable as F

import           Diagrams.Prelude

import           Plots.Themes
import           Plots.Utils
import           Plots.Types

type ColourRange = (Colour Double, Colour Double)
   
data HeatPlot n = HeatPlot
  { hData      =    [[n]]
    hColorMap  =    ColourRange
    hGrid      =    Bool
  }
type instance V (HeatMap n) = V2
type instance N (HeatMap n) = n

instance OrderedField n => Enveloped (HeatPlot n) where
  getEnvelope HeatMap {..} = getEnvelope $
    boundingBox (fromVertices [(p2 (0.5,0.5)), (p2 (0.5, ymax)), (p2 (xmax, ymax)), (p2 (ymax, 0.5))])
    where 
      ymax = (length hmata) + 0.5
      xmax = (max [length x | x <- hData]) + 0.5

instance (Typeable b, TypeableFloat n, Renderable (Path V2 n) b)
    => Plotable (HeatMap n) b where
  renderPlotable s HeatMap {..} pp =
      mconcat [rect' i j | i <- [1 .. (max [length x | x <- hData])], j<- [1 .. (length hmata)]]
        # transform t
   <> if hGrid
        then mconcat ([drawgridh] ++ [drawgridv])
               # transform t
        else mempty
    where
      drawgridh =  [fromVertices [p2 (x1, 0.5), p2 (x1, ymax)] #stroke | x1 <- [0.5, 1.5 .. ymax]]
      drawgridv =  [fromVertices [p2 (0.5, y1), p2 (xmax, y1)] #stroke | y1 <- [0.5, 1.5 .. ymax]] 
      ymax = (length hmata) + 0.5
      xmax = (max [length x | x <- hData]) + 0.5
      t  = s ^. specTrans
      ls = s ^. specScale
      rect' i j = fromVertices [p2 (imin, jmin), p2 (imin ,jmax), p2 (imax, jmax), p2 (imax, jmin)] # mapLoc closeLine # stroke # lw none # fc (colr (i,j))
                  where imin = (fromIntegral i) - 0.5
                        imax = (fromIntegral i) + 0.5
                        jmin = (fromIntegral j) - 0.5
                        jmax = (fromIntegral j) + 0.5
      colr (i,j) = createColour hColorMap (clnd !! i !! j)
      clnd = hData
      fromVertices ps
        # mapLoc closeLine
        # stroke
        # lw none
        # applyBarStyle pp
        # transform t
  defLegendPic HeatMap {..} pp
      = square 5 # applyBarStyle pp

createColour (sRGB24 x y z,sRGB24 a b c) -1 = none
createColour (sRGB24 x y z,sRGB24 a b c) n  = sRGB24 (x+(n*(a-x))) (y+(n*(b-y))) (z+(n*(c-z)))  


mkHeatMap :: (Num n, GV.Vector v a)
  => v a -> (Int,Int) -> (a -> Style V2 n) -> GHeatMap v a n
mkHeatMap v (w,h) f
  = GHeatMap
      { _heatMapVector   = v
      , _heatMapExtent   = V2 w h
      , _heatMapStart    = origin
      , _heatMapSize     = V2 1 1
      , _heatMapStyleMap = f
      }

{-
myCircle = rect 1 3 #fillTexture gradient #lw none

gradient = mkLinearGradient stops ((-1) ^& (-1)) (1 ^& 1) GradPad
stops =  mkStops [(teal, 0, 1),(orange, 1, 1)]

-- | Squares for heat map. Since some renderers leave gaps for adjacent
--   blocks, squares are overlapped to avoid this.


------------------------------------------------------------------------------

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

heatSquares :: (TypeableFloat n, Renderable (Path V2 n) b) => n -> ColourRange -> QDiagram b V2 n Any
heatSquares (V2 x y) f =
  alaf Dual F.foldMap mk ps
    # lwO 0
-- foldMap is reversed using Dual so the squares are placed in the correct order
  where
    ps = [ V2 i j | i <- [0..x-1], j <- [0..y-1] ]
    mk v@(V2 i j) =
      rect w h
        # alignBL
        # translate (fromIntegral <$> v)
        # fc (f v)
        where
          -- Squares that are not on the top left edge are slightly
          -- bigger to remove phantom gaps
          w | i == x    = 1
            | otherwise = 1.5
          h | j == y    = 1
            | otherwise = 1.5

mkHeatPlot :: (PointLike v n p, Additive v, TypeableFloat n) => (n -> p) -> ParametricPlot v n
mkParametricPlot f
  = HeatPlot
        { hData      =    [[n]]
          hColorMap  =    ColourRange
          hGrid      =    False
        }

------------------------------------------------------------------------
-- Heatmap Lenses
------------------------------------------------------------------------

class HasHeatmap a v n | a -> v n where
  heat :: Lens' a (HeatPlot v n)

  setColourMap :: Lens' a ColourRange
  setColourMap = heatmp . lens hmColorMap (\s b -> (s {hColorMap = b}))

  drawGrid :: Lens' a Bool
  drawGrid = heatmp . lens hmGrid (\s b -> (s {hGrid = b}))

instance HasVector (HeatPlot v n) v n where
  heat = id

instance HasVector (PropertiedPlot (HeatPlot v n) b) v n where
  heat = _pp

------------------------------------------------------------------------
-- Heatmap 
------------------------------------------------------------------------

data GHeatMapPlot v n a = forall s. GHeatMapPlot
  { sData :: s
  , sFold :: Fold s a
  , sPos  :: a -> Point v n
  , sHeat :: [P2 n] -> [[n]]
  , sGrid :: Bool
-- change P2 n to Point v n 
-- Look at Histogram.hs for more details
-- Extend Bool
  } deriving Typeable

type instance V (GHeatMapPlot v n a) = v
type instance N (GHeatMapPlot v n a) = n

instance (Metric v, OrderedField n) => Enveloped (GHeatMapPlot v n a) where
  getEnvelope GHeatMapPlot {..} = foldMapOf (sFold . to sPos) getEnvelope sData

instance (Typeable a, Typeable b, TypeableFloat n, Renderable (Path V2 n) b)
    => Plotable (GHeatMapPlot V2 n a) b where
  renderPlotable s HeatMapPlot {..} pp =
      F.foldMap mk ps
        # transform t
        # lwO 0.01
        # lc grey
        # withEnvelope (getEnvelope hm)
    where
      mk z@(V2 i j) =
        square 1 # fcA (pp ^. plotColourMap . ixColour (toRational $ normise n))
                 -- # translate (2*x')
                 # moveTo p
        where
          n  = hmFunction i j
          p  = hmStart .+^ (hmSize * x' + 0.5)
          x' = fromIntegral <$> z
          -- p = transform t $ sPos a
      ps = [ V2 i j | i <- [0..x-1], j <- [0..y-1] ]
      V2 x y = hmExtent
      t = s ^. specTrans
      -- p0 = apply t $ hmStart ^. _Point
      V2 vMin vMax = minmaxOf each (map (\(V2 i j) -> hmFunction i j) ps)
      normise v = (v - vMin) / (vMax - vMin)

  defLegendPic HeatMapPlot {..} pp
      = square 5 # applyBarStyle pp

------------------------------------------------------------------------
-- HeatMap Plot
------------------------------------------------------------------------

type HeatMapPlot v n = GHeatMapPlot v n (Point v n)

mkHeatMapPlot :: (PointLike v n p, F.Foldable f, Ord n, Floating n, Enum n, Num n)
              => f p -> HeatMapPlot v n
mkHeatMapPlot = mkHeatMapPlotOf folded


mkHeatMapPlotOf :: (PointLike v n p, Ord n, Floating n, Enum n, Num n)
                => Fold s p -> s -> HeatMapPlot v n
mkHeatMapPlotOf f a = GHeatMapPlot
  { sData = a
  , sFold = f . unpointLike
  , sPos  = id
  , sHEat = genHeatMap
  , sGrid = True 
  }
  
_HeatMapPlot :: (Plotable (SmoothPlot v n) b, Typeable b)
                   => Prism' (Plot b v n) (HeatMapPlot v n)
_HeatMapPlot = _Plot

-----------------------------------------------------------------------------

-- | Indexed traversal over the values of a heat map.
-- heatPoints :: IndexedTraversal' (V2 Int) (HeatMap n) Double
-- heatPoints f hm@HeatMap {..} = go 0 0 <&> \vs ->
--   let v       = U.fromListN (x*y) vs
--       hIx i j = v ! (x*j + i)
--   in  hm { hmFunction = hIx }
--   where
--     V2 x y = hmExtent
--     go !i !j
--       | i >= x    = go 0 (j+1)
--       | j >= y    = pure []
--       | otherwise = (:) <$> indexed f (V2 i j) (hmFunction i j) <*> go (i+1) j
-- {-# INLINE heatPoints #-}

-- instance HasGenericPlot (GHeatMap v a b n) where
--   genericPlot = heatMapGeneric

-- | Make a heat map from a vector. Each pixel has height and width 1 and the plot
-- mkHeatMap :: (Num n, GV.Vector v a)
--   => v a -> (Int,Int) -> (a -> Style V2 n) -> GHeatMap v a n
-- mkHeatMap v (w,h) f
--   = GHeatMap
--       { _heatMapVector   = v
--       , _heatMapExtent   = V2 w h
--       , _heatMapStart    = origin
--       , _heatMapSize     = V2 1 1
--       , _heatMapStyleMap = f
--       }

-- colourToStyle :: (Typeable n, Floating n) => AlphaColour Double -> Style V2 n
-- colourToStyle c = mempty # fcA c


-- mkGHeatMap :: (Typeable n, Floating n, P2Like Int p, GV.Vector v a, Foldable f)
--   => f (p, a) -> (Int,Int) -> a -> (a -> AlphaColour Double) -> GHeatMap v a n
-- mkGHeatMap xs (w,h) x0 f = mkHeatMap v (w,h) (colourToStyle . f)
--   where
--   v = GV.create $ do
--         mv <- GMV.replicate (w*h) x0
--         F.forM_ xs $ \(a, b) -> do
--           let P (V2 x y) = a ^. unpointLike
--               i          = y*h + x

--           GMV.write mv i b

--         return mv

-- data HeatMap

-- intHeatMap
--   :: P2 n   -- ^ start
--   -> V2 Int -- ^ extent
--   -> V2 n   -- ^ size of each box
--   -> (Int -> Int -> Double) -- ^ mapping function
--   -> HeatMap n
-- intHeatMap p0 x s f =
--   HeatMap
--     { hmFunction = f
--     , hmExtent   = x
--     , hmStart    = p0
--     , hmSize     = s
--     }

  -- { hmFunction :: Int -> Int -> Double
  -- , hmExtent   :: V2 Int  -- total x and y boxes
  -- , hmStart    :: P2 n
  -- , hmSize     :: V2 n    --

-- heatMap
--  :: Fractional n
--   => P2 n   -- ^ start
--  -> V2 n   -- ^ extent
--  -> V2 Int -- ^ number of boxes
--  -> V2 n   -- ^ size of each box
--  -> (n -> n -> Double) -- ^ mapping function
--  -> HeatMap n
-- heatMap p0@(P (V2 xp yp)) extnt ns s f' = intHeatMap p0 ns s f
--   where
--     V2 dx dy = extnt / fmap fromIntegral ns
--     f x y = f' (xp + x' * dx) (yp + y' * dy)
--       where
--         x' = fromIntegral x
--         y' = fromIntegral y
-}
