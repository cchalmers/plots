{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE RecordWildCards  #-}

module Plots.Types.Bar where
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
import           Diagrams.Coordinates.Isomorphic
import           Diagrams.Prelude

import           Plots.Themes
import Plots.Types

data HeatMap n = HeatMap
  { hmFunction :: Int -> Int -> n
  , hmExtent   :: V2 Int  -- total x and y boxes
  , hmStart    :: P2 n
  , hmSize     :: V2 n    -- width and height of each box
  } deriving Typeable

type instance V (HeatMap n) = V2
type instance N (HeatMap n) = n

instance Enveloped (HeatMap n)

-- diagrams really needs a prim to deal with copies objects like this.
instance (Typeable b, TypeableFloat n, Renderable (Path V2 n) b)
    => Plotable (HeatMap n) b where
  renderPlotable _ t HeatMap {..} pp =
    foldMap mk ps
    where
      mk x@(V2 i j) =
        rect w h # fcA (hot ^. ixColour (toRational n))
                 # moveTo p
        where
          n = hmFunction i j
          p = hmStart .+^ hmSize * fmap fromIntegral x'
          x'@(V2 i' j') = fromIntegral <$> x
          -- p = transform t $ sPos a
      ps = [ V2 i j | i <- [0..x], j <- [0..y] ]
      V2 x y = hmExtent
      V2 w h = apply t hmSize
      V2 x0 y0 = apply t $ hmStart ^. _Point

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

heatMap
  :: P2 n   -- ^ start
  -> V2 n   -- ^ extent
  -> V2 Int -- ^ number of boxes
  -> V2 n   -- ^ size of each box
  -> (n -> n -> Double) -- ^ mapping function
  -> HeatMap n
heatMap p0@(P (V2 xp yp)) extnt ns s f' = intHeatMap p0 ns s f
  where
    V2 dx dy = extnt / ns
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
