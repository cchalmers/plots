{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE AllowAmbiguousTypes          #-}


{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

module Plots.API.Scatter
  (      -- ** Scatter plot
    ScatterPlot
  , scatterPlot
  , scatterPlot'
  , scatterPlotL
  , scatterPlotOf
  , scatterPlotOf'
  , scatterPlotLOf

  , gscatterPlot
  , gscatterPlot'
  , gscatterPlotL
  -- **fold variant
  --, gscatterPlotOf
  --, gscatterPlotOf'
  --, gscatterPlotLOf
  ) where

import           Control.Lens                    hiding (( # ))
import           Control.Monad.State.Lazy
import qualified Data.Foldable as F

import           Diagrams.Coordinates.Isomorphic

import           Plots.Axis

import           Plots.Types

import           Plots.Types.Scatter
import           Plots.API

------------------------------------------------------------------------
-- Scatter plot
------------------------------------------------------------------------

-- $ scatter
-- Scatter plots display data as dots. There are several representations
-- for scatter plots for extra parameters. Scatter plots have the
-- following lenses:
--
-- @
-- * 'connectingLine' :: 'Lens'' ('ScatterPlot' v n) 'Bool' - False
-- * 'scatterTransform' :: 'Lens'' ('ScatterPlot' v n) ('Maybe' ('Point' v n -> 'T2' n)) - Nothing
-- * 'scatterStyle': 'Maybe' ('Point' v n -> 'Style' 'V2' n) - Nothing
-- @
--

-- | Add a 'ScatterPlot' to the 'AxisState' from a data set.
--
-- @
--   myaxis = r2Axis ~&
--     scatterPlot data1
-- @
--
-- === __Example__
--
-- <<plots/scatter.png#diagram=scatter&width=300>>
--
-- @
-- mydata1 = [(1,3), (2,5.5), (3.2, 6), (3.5, 6.1)]
-- mydata2 = mydata1 & each . _1 *~ 0.5
-- mydata3 = [V2 1.2 2.7, V2 2 5.1, V2 3.2 2.6, V2 3.5 5]
--
-- myaxis :: Axis B V2 Double
-- myaxis = r2Axis &~ do
--   scatterPlotL "data 1" mydata1
--   scatterPlotL "data 2" mydata2
--   scatterPlotL "data 3" mydata3
-- @

scatterPlot
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (ScatterPlot v n) b,
      F.Foldable f)
  => f p -> m ()
scatterPlot d = addPlotable (mkScatterPlot d)

-- | Make a 'ScatterPlot' and take a 'State' on the plot to alter it's
--   options
--
-- @
--   myaxis = r2Axis &~ do
--     scatterPlot' pointData1 $ do
--       connectingLine .= True
--       addLegendEntry "data 1"
-- @
scatterPlot'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (ScatterPlot v n) b,
      F.Foldable f)
  => f p -> PlotState (ScatterPlot v n) b -> m ()
scatterPlot' d = addPlotable' (mkScatterPlot d)

-- | Add a 'ScatterPlot' with the given name for the legend entry.
--
-- @
--   myaxis = r2Axis &~ do
--     scatterPlotL "blue team" pointData1
--     scatterPlotL "red team" pointData2
-- @
scatterPlotL
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (ScatterPlot v n) b,
      F.Foldable f)
  => String -> f p -> m ()
scatterPlotL l d = addPlotableL l (mkScatterPlot d)

-- Fold variants

scatterPlotOf
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (ScatterPlot v n) b)
  => Fold s p -> s -> m ()
scatterPlotOf f s = addPlotable (mkScatterPlotOf f s)

scatterPlotOf'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (ScatterPlot v n) b)
  => Fold s p -> s -> PlotState (ScatterPlot v n) b -> m ()
scatterPlotOf' f s = addPlotable' (mkScatterPlotOf f s)

scatterPlotLOf
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (ScatterPlot v n) b)
  => String -> Fold s p -> s -> m ()
scatterPlotLOf l f s = addPlotableL l (mkScatterPlotOf f s)

------------------------------------------------------------------------
-- Bubble plot -- ??
------------------------------------------------------------------------

-- $ bubble
-- Scatter plots with extra numeric parameter. By default the extra
-- parameter is the scale of the marker but this can be changed.

-- bubblePlot :: (PointLike (BaseSpace v) n p, R2Backend b n, Plotable (P.ScatterPlot v n) b, F.Foldable f)
--             => f (n,p) -> AxisState b v n
-- bubblePlot d = axisPlots <>= [P.Plot (P.mkBubblePlot d) def]

-- bubblePlot' :: (PointLike (BaseSpace v) n p, R2Backend b n, Plotable (P.ScatterPlot v n) b, F.Foldable f)
--             => f (n,p) -> AxisState b v n
-- bubblePlot' d s = axisPlots <>= [P.Plot (execState s $ P.mkBubblePlot d) def]

------------------------------------------------------------------------
-- GScatterPlot
------------------------------------------------------------------------

gscatterPlot
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (GScatterPlot v n a) b,
      F.Foldable f)
  => f a -> (a -> p) -> m ()
gscatterPlot d pf = addPlotable (mkGScatterPlot d pf)

gscatterPlot'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (GScatterPlot v n a) b,
      F.Foldable f)
  => f a -> (a -> p) -> PlotState (GScatterPlot v n a) b -> m ()
gscatterPlot' d pf = addPlotable' (mkGScatterPlot d pf)


gscatterPlotL
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (GScatterPlot v n a) b,
      F.Foldable f)
  => String -> f a -> (a -> p) -> m ()
gscatterPlotL l d pf = addPlotableL l (mkGScatterPlot d pf)
