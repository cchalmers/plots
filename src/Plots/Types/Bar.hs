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
  , barWidth
  , barSpacing
  , verticleBars
  ) where

import Control.Lens     hiding (transform, ( # ))
import Data.Default
import Data.Typeable
import Data.Foldable (Foldable, foldMap, toList)
import Diagrams.Prelude

-- import Plots.Themes
import Plots.Types

data BarPlot n = BarPlot
  { barData       :: [(n,[n])] -- data for bars
  , _barWidth     :: n         -- total width of bars for one 'bit'
  , _barSpacing   :: n         -- gap between multibars in same value
  , _verticleBars :: Bool    -- whether the bars are verticle
  } deriving Typeable

type instance V (BarPlot n) = V2
type instance N (BarPlot n) = n

makeLenses ''BarPlot

instance OrderedField n => Enveloped (BarPlot n) where
  getEnvelope = mempty

instance (Typeable b, TypeableFloat n, Renderable (Path V2 n) b)
    => Plotable (BarPlot n) b where
  renderPlotable _ t d _pp = drawBarPlot d # transform t

instance Fractional n => Default (BarPlot n) where
  def = BarPlot
          { barData       = []
          , _barWidth     = 0.5
          , _barSpacing   = 0.1
          , _verticleBars = True
          }

-- TODO: work out a nice way to get different colours for multi-bar plots.

drawBarPlot :: (TypeableFloat n, Renderable (Path V2 n) b) => BarPlot n -> QDiagram b V2 n Any
drawBarPlot bp = foldMap makeBar (barData bp)
  where
    tW = bp^.barWidth
    δ  = bp^.barSpacing
    --
    makeBar (_,[]) = mempty
    makeBar (x,bs) = ifoldMap mkBar bs
      where
        mkBar i h = rect w h
                      # alignBL
                      # translateX (x + fromIntegral i * (δ + w))
        n = fromIntegral $ length bs
        w = recip n * (tW - δ * (n - 1))

-- instance (Typeable b, Renderable (Path R2) b) => Plotable (BarPlot b) b R2 where
--   plot _r _ t = transform t . drawBarPlot

simpleBarPlot :: (TypeableFloat n, Foldable f) => f n -> BarPlot n
simpleBarPlot (toList -> xs) = def { barData = imap f xs }
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


