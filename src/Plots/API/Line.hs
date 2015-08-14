{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE AllowAmbiguousTypes          #-}


{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

module Plots.API.Line
  ( LinePlot
  , linePlot
  , linePlot'
  , linePlot''
  , linePlotL
  , linePlotOf
  , linePlotOf'
  , linePlotLOf

  , pathPlot''

  --, createstep
  , stepPlot
  , stepPlot'
  , stepPlotL

  , glinePlot
  , glinePlot'
  , glinePlotL
  ) where

import           Control.Lens                    hiding (( # ))
import           Control.Monad.State.Lazy
import           Data.Typeable
import qualified Data.Foldable as F

import           Diagrams.Coordinates.Isomorphic
import           Diagrams.Prelude

import           Plots.Axis

import           Plots.Types

import           Plots.Types.Line

import           Plots.API

------------------------------------------------------------------------
-- Line plot
------------------------------------------------------------------------

-- $ line
-- line plots display data as dots. There are several representations
-- for line plots for extra parameters. Line plots have the
-- following lenses:
--
-- @
-- * 'dotsonPoint' :: 'Lens'' ('LinePlot' v n) 'Bool' - False
-- * 'lineStyle'   :: 'Maybe' ('Point' v n -> 'Style' 'V2' n) - Nothing
-- @
--
-- | Add a 'LinePlot' to the 'AxisState' from a data set.
--
-- @
--   myaxis = r2Axis ~&
--     linePlot data1
-- @
-- === __Example__
--
-- <<plots/line.png#diagram=line&width=300>>
--
-- @
--
-- myaxis :: Axis B V2 Double
-- myaxis = r2Axis &~ do
--          linePlot  mydata1 
--          linePlot' mydata2 $ do
--               addLegendEntry "data 2"
--               plotColor .= black
--               dotsonPoint .= False
--          linePlotL "data 3" mydata3
--
--          axisPlots . each . _LinePlot' . dotsonPoint .= True
--
-- @

linePlot
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (LinePlot v n) b,
      F.Foldable f)
  => f p -> m ()
linePlot d = addPlotable (mkLinePlot d)

-- | Make a 'LinePlot' and take a 'State' on the plot to alter it's
--   options
--
linePlot'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (LinePlot v n) b,
      F.Foldable f)
  => f p -> PlotState (LinePlot v n) b -> m ()
linePlot' d = addPlotable' (mkLinePlot d)

-- | Add a 'LinePlot' with the given name for the legend entry.
--
-- @
--   myaxis = r2Axis &~ do
--     linePlotL "blue team" pointData1
--     linePlotL "red team" pointData2
-- @
linePlotL
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (LinePlot v n) b,
      F.Foldable f)
  => String -> f p -> m ()
linePlotL l d = addPlotableL l (mkLinePlot d)


-- | mkTrail version of line plot

linePlot''
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      R2Backend b n,
      Plotable (Path v n) b,
      F.Foldable f)
  => f p -> m ()
linePlot'' d = addPlotable (mkPath $ Identity d)

pathPlot'' :: (R2Backend b n, MonadState (Axis b V2 n) m) => Path V2 n -> m ()
pathPlot'' = addPlotable

-- Fold variants

linePlotOf
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (LinePlot v n) b)
  => Fold s p -> s -> m ()
linePlotOf f s = addPlotable (mkLinePlotOf f s)

linePlotOf'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (LinePlot v n) b)
  => Fold s p -> s -> PlotState (LinePlot v n) b -> m ()
linePlotOf' f s = addPlotable' (mkLinePlotOf f s)

linePlotLOf
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (LinePlot v n) b)
  => String -> Fold s p -> s -> m ()
linePlotLOf l f s = addPlotableL l (mkLinePlotOf f s)

------------------------------------------------------------------------
--Step
------------------------------------------------------------------------

stepPlot :: (RealFloat n, Typeable n, Typeable b, Renderable (Path V2 n) b,
             MonadState (Axis b c n) m, BaseSpace c ~ V2)
         => [(n, n)] -> m ()
stepPlot  a   = linePlot (createStepData a)

stepPlot' :: (RealFloat n, Typeable n, Typeable b, Renderable (Path V2 n) b,
              MonadState (Axis b c n) m, BaseSpace c ~ V2) =>
             [(n, n)] -> PlotState (LinePlot V2 n) b -> m ()
stepPlot' a   = linePlot' (createStepData a)

stepPlotL :: (RealFloat n, Typeable n, Typeable b, Renderable (Path V2 n) b,
              MonadState (Axis b c n) m, BaseSpace c ~ V2) =>
             String -> [(n, n)] -> m ()
stepPlotL l a = linePlotL l (createStepData a)

------------------------------------------------------------------------
-- General Line Plot
------------------------------------------------------------------------


glinePlot
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (GLinePlot v n a) b,
      F.Foldable f)
  => f a -> (a -> p) -> m ()
glinePlot d pf = addPlotable (mkGLinePlot d pf)

glinePlot'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (GLinePlot v n a) b,
      F.Foldable f)
  => f a -> (a -> p) -> PlotState (GLinePlot v n a) b -> m ()
glinePlot' d pf = addPlotable' (mkGLinePlot d pf)

glinePlotL
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (GLinePlot v n a) b,
      F.Foldable f)
  => String -> f a -> (a -> p) -> m ()
glinePlotL l d pf = addPlotableL l (mkGLinePlot d pf)
