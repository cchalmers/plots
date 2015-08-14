{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE AllowAmbiguousTypes          #-}

{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

module Plots.API.Histogram
  ( -- * Histogram
    histogramPlot
  , histogramPlot'
  , histogramPlotL
    -- * Fold variant histogram
  , histogramPlotOf
  , histogramPlotOf'
  , histogramPlotLOf
  ) where

import           Control.Lens                    hiding (( # ))
import           Control.Monad.State.Lazy

import qualified Data.Foldable as F

import           Diagrams.Coordinates.Isomorphic

import           Plots.Axis
import           Plots.Types
import           Plots.API
import           Plots.Types.Histogram

------------------------------------------------------------------------
-- Histogram
------------------------------------------------------------------------

-- $ histogram
-- Histograms display data as barplot of x data, bin y data.
-- Box plots have the following lenses:
--
-- @
-- * 'setBin' :: 'Lens'' ('BoxPlot' v n) 'Double' - 10
-- @

-- | Add a 'HistogramPlot' to the 'AxisState' from a data set.
--
-- @
--   myaxis = r2Axis ~&
--     histogramPlot data1
-- @
--
-- === __Example__
--
-- <<plots/histogram.png#diagram=histogram&width=300>>
--
-- @
-- fillOpacity = barStyle . mapped . _opacity
--
-- myaxis :: Axis B V2 Double
-- myaxis = r2Axis &~ do
--  histogramPlot' mydata1 $ do 
--     addLegendEntry "histogram"
--     plotColor .= blue
--     fillOpacity .= 0.5
-- @

histogramPlot
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (HistogramPlot v n) b,
      F.Foldable f, Enum n)
  => f p -> m ()
histogramPlot d = addPlotable (mkHistogramPlot d)

-- | Make a 'HistogramPlot' and take a 'State' on the plot to alter it's
--   options
--
-- @
--   myaxis = r2Axis &~ do
--     histogramPlot' pointData1 $ do
--       setBin .= 30
--       addLegendEntry "data 1"
-- @

histogramPlot'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (HistogramPlot v n) b,
      F.Foldable f, Enum n)
  => f p -> PlotState (HistogramPlot v n) b -> m ()
histogramPlot' d = addPlotable' (mkHistogramPlot d)

-- | Add a 'HistogramPlot' with the given name for the legend entry.
--
-- @
--   myaxis = r2Axis &~ do
--     histogramPlotL "blue team" pointData1
--     histogramPlotL "red team" pointData2
-- @

histogramPlotL
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (HistogramPlot v n) b,
      F.Foldable f, Enum n)
  => String -> f p -> m ()
histogramPlotL l d = addPlotableL l (mkHistogramPlot d)

-- Fold variants

histogramPlotOf
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (HistogramPlot v n) b, Enum n)
  => Fold s p -> s -> m ()
histogramPlotOf f s = addPlotable (mkHistogramPlotOf f s)

histogramPlotOf'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (HistogramPlot v n) b, Enum n)
  => Fold s p -> s -> PlotState (HistogramPlot v n) b -> m ()
histogramPlotOf' f s = addPlotable' (mkHistogramPlotOf f s)

histogramPlotLOf
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (HistogramPlot v n) b, Enum n)
  => String -> Fold s p -> s -> m ()
histogramPlotLOf l f s = addPlotableL l (mkHistogramPlotOf f s)
