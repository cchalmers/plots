{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Plots.Types.Bar
  ( BarPlot
  , simpleBarPlot
    -- * Prism
  -- , _BarPlot


    -- * Lenses
  , barPlotBars
  , barPlotWidth
  , barPlotSpacing
  , barPlotIsVerticle
  ) where

import Control.Lens     hiding (transform, ( # ))
import Data.Default
import Data.Typeable
import Data.Foldable (Foldable, foldMap, toList)
import Diagrams.Prelude

-- import Plots.Themes
import Plots.Types

data BarPlot n = BarPlot
  { _barPlotBars     :: [(n,[n])] -- data for bars
  , _barPlotWidth    :: n         -- total width of bars for one 'bit'
  , _barPlotSpacing  :: n         -- gap between multibars in same value
  , _barPlotIsVerticle :: Bool    -- whether the bars are verticle
  } deriving Typeable

type instance V (BarPlot n) = V2
type instance N (BarPlot n) = n

makeLenses ''BarPlot

instance (Typeable b, TypeableFloat n, Renderable (Path V2 n) b)
    => Plotable (BarPlot n) b where
  renderPlotable _gp _ _t = drawBarPlot

instance Fractional n => Default (BarPlot n) where
  def = BarPlot
          { _barPlotBars       = []
          , _barPlotWidth      = 0.5
          , _barPlotSpacing    = 0.1
          , _barPlotIsVerticle = True
          }

-- TODO: work out a nice way to get different colours for multi-bar plots.

drawBarPlot :: (TypeableFloat n, Renderable (Path V2 n) b) => BarPlot n -> QDiagram b V2 n Any
drawBarPlot bp = foldMap makeBar (bp^.barPlotBars)
  where
    tW = bp^.barPlotWidth
    δ  = bp^.barPlotSpacing
    --
    makeBar (_,[]) = mempty
    makeBar (x,bs) = ifoldMap mkBar bs
      where
        mkBar i h = rect w h
                      # alignBL
                      # translateX (x + fromIntegral i * (δ + w))
        n = fromIntegral $ length bs
        w = recip n * (tW - δ * (n - 1))

    -- makeBar i [h1,h2] = rect (bW / 2) h1
    --                       # centerX
    --                       # alignB
    --                       # translateX (fromIntegral i + 1 - 0.15)
    --                  <> rect (bW / 2) h2
    --                       # fc orange
    --                       # centerX
    --                       # alignB
    --                       # translateX (fromIntegral i + 1 + 0.15)
    -- makeBar _ _ = mempty

-- instance (Typeable b, Renderable (Path R2) b) => Plotable (BarPlot b) b R2 where
--   plot _r _ t = transform t . drawBarPlot

simpleBarPlot :: (TypeableFloat n, Foldable f) => f n -> BarPlot n
simpleBarPlot (toList -> xs) = def & barPlotBars .~ imap f xs
  where
    f i h = (fromIntegral i + 1, [h])


-- _BarPlot :: Plotable (BarPlot n) => Prism' (Plot b V2 n) (BarPlot n)
-- _BarPlot = _Plot

------------------------------------------------------------------------
-- Histogram
------------------------------------------------------------------------


-- data Histogram n a = forall s. Histogram
--   { histogramData  :: s
--   , histogramFold  :: Fold s a
--   , histogramLowerLimit :: Maybe n
--   , histogramUpperLimit :: Maybe n
--   }
--
-- mkHistogramOf :: Fold s n -> s -> BarPlot n
-- mkHistogramOf f as =



------------------------------------------------------------------------
-- Histogram
------------------------------------------------------------------------



