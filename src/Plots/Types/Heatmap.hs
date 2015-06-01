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
import           Data.Foldable                   as F
import qualified Data.Vector.Generic             as GV
import qualified Data.Vector.Generic.Mutable     as GMV
import qualified Data.Vector.Unboxed as U
import           Data.Vector.Unboxed ((!))
import           Diagrams.Coordinates.Isomorphic
import           Diagrams.Prelude

import           Plots.Themes
import Plots.Types

data HeatMap n = HeatMap
  { hmFunction :: Int -> Int -> Double
  , hmExtent   :: V2 Int  -- total x and y boxes
  , hmStart    :: P2 n
  , hmSize     :: V2 n    -- width and height of each box
  } deriving Typeable
-- I'm still unsure of the "right" way to do this. On the one hand, by
-- restricting it to int users can read off data from an array without
-- any problems. On the other hand it's not an ideal representation for
-- heat maps of functions. Maybe we need two primitives.

type instance V (HeatMap n) = V2
type instance N (HeatMap n) = n

instance OrderedField n => Enveloped (HeatMap n) where
  getEnvelope HeatMap {..} = getEnvelope $
    fromCorners hmStart (hmStart .+^ fmap fromIntegral hmExtent)

-- diagrams really needs a prim to deal with multiple objects like this.
instance (Typeable b, TypeableFloat n, Renderable (Path V2 n) b)
    => Plotable (HeatMap n) b where
  renderPlotable s hm@HeatMap {..} _pp =
      foldMap mk ps
        # transform t
        # lwO 0.5
        # withEnvelope (getEnvelope hm)
    where
      mk z@(V2 i j) =
        square 1 # fcA (hot ^. ixColour (toRational $ normise n))
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

-- | 'V2 min max'
minmaxOf :: (Fractional a, Ord a) => Getting (Endo (Endo (V2 a))) s a -> s -> V2 a
minmaxOf l = foldlOf' l (\(V2 mn mx) a -> V2 (min mn a) (max mx a)) (V2 (1/0) (-1/0))
      -- (\acc a -> acc <**> V2 min max ?? a)
-- V2 is used instead of a tuple because V2 is strict.
{-# INLINE minmaxOf #-}

-- | Indexed traversal over the values of a heat map.
heatPoints :: IndexedTraversal' (V2 Int) (HeatMap n) Double
heatPoints f hm@HeatMap {..} = go 0 0 <&> \vs ->
  let v       = U.fromListN (x*y) vs
      hIx i j = v ! (x*j + i)
  in  hm { hmFunction = hIx }
  where
    V2 x y = hmExtent
    go !i !j
      | i >= x    = go 0 (j+1)
      | j >= y    = pure []
      | otherwise = (:) <$> indexed f (V2 i j) (hmFunction i j) <*> go (i+1) j
{-# INLINE heatPoints #-}

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

colourToStyle :: (Typeable n, Floating n) => AlphaColour Double -> Style V2 n
colourToStyle c = mempty # fcA c


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

intHeatMap
  :: P2 n   -- ^ start
  -> V2 Int -- ^ extent
  -> V2 n   -- ^ size of each box
  -> (Int -> Int -> Double) -- ^ mapping function
  -> HeatMap n
intHeatMap p0 x s f =
  HeatMap
    { hmFunction = f
    , hmExtent   = x
    , hmStart    = p0
    , hmSize     = s
    }

  -- { hmFunction :: Int -> Int -> Double
  -- , hmExtent   :: V2 Int  -- total x and y boxes
  -- , hmStart    :: P2 n
  -- , hmSize     :: V2 n    --

heatMap
  :: Fractional n
  => P2 n   -- ^ start
  -> V2 n   -- ^ extent
  -> V2 Int -- ^ number of boxes
  -> V2 n   -- ^ size of each box
  -> (n -> n -> Double) -- ^ mapping function
  -> HeatMap n
heatMap p0@(P (V2 xp yp)) extnt ns s f' = intHeatMap p0 ns s f
  where
    V2 dx dy = extnt / fmap fromIntegral ns
    f x y = f' (xp + x' * dx) (yp + y' * dy)
      where
        x' = fromIntegral x
        y' = fromIntegral y


-- data GHeatMap v a n = GHeatMap
--   { _heatMapVector   :: v a
--   , _heatMapExtent   :: V2 Int  -- total width and height
--   , _heatMapStart    :: P2 n
--   , _heatMapSize     :: V2 n    -- width and height of each box
--   , _heatMapStyleMap :: a -> Style V2 n
--   } deriving Typeable

-- type instance V (GHeatMap v a n) = V2
-- type instance N (GHeatMap v a n) = n

-- makeLenses ''GHeatMap

-- -- instance HasGenericPlot (GHeatMap v a b n) where
-- --   genericPlot = heatMapGeneric

-- -- | Make a heat map from a vector. Each pixel has height and width 1 and the plot
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



-- --
-- -- instance (Typeable b, TypeableFloat n, Renderable (Path V2 n) b)
-- --     => Plotable (HeatMap b n) where
-- --   plot _ tv l t2 = drawHeatMap
-- --
-- --
-- --
-- -- simpleHeatMap :: (Renderable (Path V2 n) b, TypeableFloat n, Foldable f) => f n -> HeatMap b n
-- -- simpleHeatMap (toList -> xs) = def & HeatMapBars .~ imap f xs
-- --   where
-- --     f i h = (fromIntegral i + 1, [h])
-- --
-- -- _HeatMap :: Plotable (HeatMap b n) => Prism' (Plot b V2 n) (HeatMap b n)
-- -- _HeatMap = _Plot
-- --
-- --
