{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE FunctionalDependencies    #-}

{-# LANGUAGE StandaloneDeriving        #-}

module Plots.Types.Others
  ( -- * Vertical line range
    createlinerangev
  , linerangevPlot
  , linerangevPlot'
  -- , linerangevPlotL
  , linerangevPlotwithPoint

    -- * Horizontal line range
  , createlinerangeh
  , linerangehPlot
  , linerangehPlot'
  -- , linerangehPlotL
  , linerangehPlotwithPoint

    -- * Vertical errorbar
  , errorbarvPlot
  , errorbarvPlotwithPoint

    -- * Horizontal errorbar
  , errorbarhPlot
  , errorbarhPlotwithPoint

    -- * Vertical crossbar
  , crossbarvPlot
  , crossbarvPlotwithPoint

    -- * Horizontal crossbar
  , crossbarhPlot
  , crossbarhPlotwithPoint

    -- * Boxplot
  , boxplotvPlot
  , boxplothPlot
  ) where

import           Control.Lens                    hiding (lmap, none, transform,
                                                  ( # ))
import           Control.Monad.State.Lazy

-- import qualified Data.Foldable                   as F
import           Data.Typeable
-- import           Data.List
-- import           Data.Function

import           Diagrams.Prelude
-- import           Diagrams.Coordinates.Isomorphic

import           Plots.Style
import           Plots.Types
import           Plots.API
-- import           Plots.Axis
import           Plots.Types.Line
import           Plots.Types.Scatter

------------------------------------------------------------------------
-- Linerange Vertical
------------------------------------------------------------------------
createlinerangev :: (Double, Double) -> Double -> [(Double, Double)]
createlinerangev (a, b) s = [(a+(s/2), b), (a-(s/2), b)]

linerangevPlot :: (Typeable b, Renderable (Path V2 Double) b,
                   MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                  (Double, Double) -> Double -> m ()
linerangevPlot a s  = linePlot' (createlinerangev a s)

linerangevPlot' :: (Typeable b, Renderable (Path V2 Double) b,
                    MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                   (Double, Double) -> Double -> m ()
linerangevPlot' a s  = linePlot' (createlinerangev a s)

-- linerangevPlotL :: (Typeable b, Renderable (Path V2 Double) b,
--                     MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
--                    String -> (Double, Double) -> Double -> m ()
-- linerangevPlotL l a s = linePlotL l (createlinerangev a s)

linerangevPlotwithPoint :: (Typeable b, Renderable (Path V2 Double) b,
                            MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                           (Double, Double) -> Double -> m ()
linerangevPlotwithPoint a s = do
     linePlot (createlinerangev a s) $ do
        plotColor .= purple
        -- addLegendEntry "data 1"
     scatterPlot [a] $ do
        plotColor .= purple
        plotMarker %= scale 2

------------------------------------------------------------------------
-- Linerange Horizontal
------------------------------------------------------------------------
createlinerangeh :: (Double, Double) -> Double -> [(Double, Double)]
createlinerangeh (a, b) s = [(a, b+(s/2)), (a, b-(s/2))]

linerangehPlot :: (Typeable b, Renderable (Path V2 Double) b,
                   MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                  (Double, Double) -> Double -> m ()
linerangehPlot a s  = linePlot' (createlinerangeh a s)

linerangehPlot' :: (Typeable b, Renderable (Path V2 Double) b,
                    MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                   (Double, Double) -> Double -> m ()
linerangehPlot' a s  = linePlot' (createlinerangeh a s)

-- linerangehPlotL :: (Typeable b, Renderable (Path V2 Double) b,
--                     MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
--                    String -> (Double, Double) -> Double -> m ()
-- linerangehPlotL l a s = linePlotL l (createlinerangeh a s)

linerangehPlotwithPoint :: (Typeable b, Renderable (Path V2 Double) b,
                            MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                           (Double, Double) -> Double -> m ()
linerangehPlotwithPoint a s = do
     linePlot (createlinerangeh a s) $ do
        plotColor .= purple
        -- addLegendEntry "data 1"
     scatterPlot [a] $ do
        plotColor .= purple
        plotMarker %= scale 2

------------------------------------------------------------------------
-- Errorbar Vertical
------------------------------------------------------------------------

errorbarvPlot :: (Typeable b, Renderable (Path V2 Double) b,
                  MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                 (Double, Double) -> Double -> Double -> m ()
errorbarvPlot (a,b) s h = do
              linePlot (createlinerangev (a, b) s) $ do
                 plotColor .= red
                 -- addLegendEntry "data 1"
              linePlot (createlinerangeh (a+(s/2), b) h) $ do
                 plotColor .= red
              linePlot (createlinerangeh (a-(s/2), b) h) $ do
                 plotColor .= red

errorbarvPlotwithPoint :: (Typeable b, Renderable (Path V2 Double) b,
                           MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                          (Double, Double) -> Double -> Double -> m ()
errorbarvPlotwithPoint (a,b) s h = do
              linePlot (createlinerangev (a, b) s) $ do
                 plotColor .= blue
                 -- addLegendEntry "data 1"
              linePlot (createlinerangeh (a+(s/2), b) h) $ do
                 plotColor .= blue
              linePlot (createlinerangeh (a-(s/2), b) h) $ do
                 plotColor .= blue
              scatterPlot [(a,b)] $ do
                 plotColor .= blue



------------------------------------------------------------------------
-- Errorbar Horizontal
------------------------------------------------------------------------

errorbarhPlot :: (Typeable b, Renderable (Path V2 Double) b,
                  MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                 (Double, Double) -> Double -> Double -> m ()
errorbarhPlot (a,b) s h = do
              linePlot (createlinerangeh (a, b) s) $ do
                 plotColor .= red
                 -- addLegendEntry "data n"
              linePlot (createlinerangev (a, b+(s/2)) h) $ do
                 plotColor .= red
              linePlot (createlinerangev (a, b-(s/2)) h) $ do
                 plotColor .= red

errorbarhPlotwithPoint :: (Typeable b, Renderable (Path V2 Double) b,
                           MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                          (Double, Double) -> Double -> Double -> m ()
errorbarhPlotwithPoint (a,b) s h = do
              linePlot (createlinerangeh (a, b) s) $ do
                 plotColor .= blue
                 -- addLegendEntry "data n"
              linePlot (createlinerangev (a, b+(s/2)) h) $ do
                 plotColor .= blue
              linePlot (createlinerangev (a, b-(s/2)) h) $ do
                 plotColor .= blue
              scatterPlot [(a,b)] $ do
                 plotColor .= blue

------------------------------------------------------------------------
-- Crossbar Vertical
------------------------------------------------------------------------

crossbarvPlot :: (Typeable b, Renderable (Path V2 Double) b,
                  MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                 (Double, Double) -> Double -> Double -> m ()
crossbarvPlot (a,b) s h = do
              linePlot (createlinerangeh (a, b) h) $ do
                 plotColor .= red
                 -- addLegendEntry "data n"
              linePlot (createlinerangev (a, b+(h/2)) s) $ do
                 plotColor .= red
              linePlot (createlinerangev (a, b-(h/2)) s) $ do
                 plotColor .= red
              linePlot (createlinerangeh (a+(s/2), b) h) $ do
                 plotColor .= red
              linePlot (createlinerangeh (a-(s/2), b) h) $ do
                 plotColor .= red

crossbarvPlotwithPoint :: (Typeable b, Renderable (Path V2 Double) b,
                           MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                          (Double, Double) -> Double -> Double -> m ()
crossbarvPlotwithPoint (a,b) s h = do
              linePlot (createlinerangeh (a, b) h) $ do
                 plotColor .= blue
                 -- addLegendEntry "data n"
              linePlot (createlinerangev (a, b+(h/2)) s) $ do
                 plotColor .= blue
              linePlot (createlinerangev (a, b-(h/2)) s) $ do
                 plotColor .= blue
              linePlot (createlinerangeh (a+(s/2), b) h) $ do
                 plotColor .= blue
              linePlot (createlinerangeh (a-(s/2), b) h) $ do
                 plotColor .= blue
              scatterPlot [(a,b)] $ do
                 plotColor .= blue

------------------------------------------------------------------------
-- Crossbar Horizontal  --options for colour and legends seperate
------------------------------------------------------------------------

crossbarhPlot :: (Typeable b, Renderable (Path V2 Double) b,
                  MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                 (Double, Double) -> Double -> Double -> m ()
crossbarhPlot (a,b) s h = do
              linePlot (createlinerangev (a, b) s) $ do
                 plotColor .= red
                 -- addLegendEntry "data n"
              linePlot (createlinerangev (a, b+(h/2)) s) $ do
                 plotColor .= red
              linePlot (createlinerangev (a, b-(h/2)) s) $ do
                 plotColor .= red
              linePlot (createlinerangeh (a+(s/2), b) h) $ do
                 plotColor .= red
              linePlot (createlinerangeh (a-(s/2), b) h) $ do
                 plotColor .= red

crossbarhPlotwithPoint :: (Typeable b, Renderable (Path V2 Double) b,
                           MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                          (Double, Double) -> Double -> Double -> m ()
crossbarhPlotwithPoint (a,b) s h = do
              linePlot (createlinerangev (a, b) s) $ do
                 plotColor .= blue
                 -- addLegendEntry "data n"
              linePlot (createlinerangev (a, b+(h/2)) s) $ do
                 plotColor .= blue
              linePlot (createlinerangev (a, b-(h/2)) s) $ do
                 plotColor .= blue
              linePlot (createlinerangeh (a+(s/2), b) h) $ do
                 plotColor .= blue
              linePlot (createlinerangeh (a-(s/2), b) h) $ do
                 plotColor .= blue
              scatterPlot [(a,b)] $ do
                 plotColor .= blue

------------------------------------------------------------------------
-- Boxplot Vertical
------------------------------------------------------------------------

boxplotvPlot :: (Typeable b, Renderable (Path V2 Double) b,
                 MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                (Double, Double) -> Double -> Double -> Double -> m ()
boxplotvPlot (a,b) s1 s2 h = do
                             crossbarhPlot (a,b) s1 h
                             linePlot (createlinerangeh (a-(s1/2), b) (s2-s1)) $ do
                                 plotColor .= red
                             linePlot (createlinerangeh (a+(s1/2), b) (s2-s1)) $ do
                                 plotColor .= red


------------------------------------------------------------------------
-- Boxplot Horizontal
------------------------------------------------------------------------

boxplothPlot :: (Typeable b, Renderable (Path V2 Double) b,
                 MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                (Double, Double) -> Double -> Double -> Double -> m ()
boxplothPlot (a,b) s h1 h2= do
                            crossbarhPlot (a,b) s h1
                            linePlot (createlinerangeh (a, b1) hmean) $ do
                                 plotColor .= red
                            linePlot (createlinerangeh (a, b2) hmean) $ do
                                 plotColor .= red
                            where b1 = b + (h1/2) + (h2/2)
                                  b2 = b - (h1/2) - (h2/2)
                                  hmean = h2-h1


------------------------------------------------------------------------
-- Bar Plot
------------------------------------------------------------------------

-- $bar
-- Bar plots are different in that you often want to specify the axis
-- name along with the data.

-- barPlot :: (R2Backend, Plotable (Path v n) b, F.Foldable f)
--         => f (String, n) -> AxisState b v n
-- barPlot d = addPlotable $ P.mkBarPlot' d

-- barPlot' :: (R2Backend, Plotable (Path v n) b, F.Foldable f)
--         => f (String, n) -> AxisState b v n
-- barPlot' d s = addPlotable (P.mkBarPlot d) s

-- barPlotL :: (R2Backend, Plotable (Path v n) b, F.Foldable f)
--         => String -> f (String, n) -> AxisState b v n
-- barPlotL s d = addPlotableL s (P.mkBarPlot d)
