{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE AllowAmbiguousTypes          #-}


{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

module Plots.API.Smooth
  (  smoothPlot
   , smoothPlot'
   , smoothPlotL
   , smoothPlotOf
   , smoothPlotOf'
   , smoothPlotOfL
  ) where

import           Control.Lens                    hiding (( # ))
import           Control.Monad.State.Lazy
import qualified Data.Foldable as F

import           Diagrams.Coordinates.Isomorphic
import           Diagrams.Prelude

import           Plots.Axis

import           Plots.Types

import           Plots.Types.Smooth
import           Plots.API

------------------------------------------------------------------------
-- Smooth Plot
------------------------------------------------------------------------

smoothPlot
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (SmoothPlot v n) b,
      F.Foldable f ,
      Enum n, TypeableFloat n)
  => f p -> m ()
smoothPlot d = addPlotable (mkSmoothPlot d)

smoothPlot'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (SmoothPlot v n) b,
      F.Foldable f ,
      Enum n, TypeableFloat n)
  => f p -> PlotState (SmoothPlot v n) b -> m ()
smoothPlot' d = addPlotable' (mkSmoothPlot d)

smoothPlotL
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (SmoothPlot v n) b,
      F.Foldable f ,
      Enum n, TypeableFloat n)
  => String -> f p  -> m ()
smoothPlotL l d = addPlotableL l (mkSmoothPlot d)

smoothPlotOf
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (SmoothPlot v n) b,
      Enum n, TypeableFloat n)
  => Fold s p -> s -> m ()
smoothPlotOf f s = addPlotable (mkSmoothPlotOf f s)

smoothPlotOf'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (SmoothPlot v n) b,
      Enum n, TypeableFloat n)
  => Fold s p -> s -> PlotState (SmoothPlot v n) b -> m ()
smoothPlotOf' f s = addPlotable' (mkSmoothPlotOf f s)

smoothPlotOfL
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (SmoothPlot v n) b,
      Enum n, TypeableFloat n)
  => String -> Fold s p -> s -> m ()
smoothPlotOfL l f s = addPlotableL l (mkSmoothPlotOf f s)
