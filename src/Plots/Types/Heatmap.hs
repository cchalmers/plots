{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

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

-- import Plots.Themes
-- import Plots.Types

data GHeatMap v a n = GHeatMap
  { _heatMapVector   :: v a
  , _heatMapExtent   :: V2 Int  -- total width and height
  , _heatMapStart    :: P2 n
  , _heatMapSize     :: V2 n    -- width and height of each box
  , _heatMapStyleMap :: a -> Style V2 n
  } deriving Typeable

type instance V (GHeatMap v a n) = V2
type instance N (GHeatMap v a n) = n

makeLenses ''GHeatMap

-- instance HasGenericPlot (GHeatMap v a b n) where
--   genericPlot = heatMapGeneric

-- | Make a heat map from a vector. Each pixel has height and width 1 and the plot
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

colourToStyle :: (Typeable n, Floating n) => AlphaColour Double -> Style V2 n
colourToStyle c = mempty # fcA c


mkGHeatMap :: (Typeable n, Floating n, P2Like Int p, GV.Vector v a, Foldable f)
  => f (p, a) -> (Int,Int) -> a -> (a -> AlphaColour Double) -> GHeatMap v a n
mkGHeatMap xs (w,h) x0 f = mkHeatMap v (w,h) (colourToStyle . f)
  where
  v = GV.create $ do
        mv <- GMV.replicate (w*h) x0
        F.forM_ xs $ \(a, b) -> do
          let P (V2 x y) = a ^. unpointLike
              i          = y*h + x

          GMV.write mv i b

        return mv



--
-- instance (Typeable b, TypeableFloat n, Renderable (Path V2 n) b)
--     => Plotable (HeatMap b n) where
--   plot _ tv l t2 = drawHeatMap
--
--
--
-- simpleHeatMap :: (Renderable (Path V2 n) b, TypeableFloat n, Foldable f) => f n -> HeatMap b n
-- simpleHeatMap (toList -> xs) = def & HeatMapBars .~ imap f xs
--   where
--     f i h = (fromIntegral i + 1, [h])
--
-- _HeatMap :: Plotable (HeatMap b n) => Prism' (Plot b V2 n) (HeatMap b n)
-- _HeatMap = _Plot
--
--
