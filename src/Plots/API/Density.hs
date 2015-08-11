{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE AllowAmbiguousTypes          #-}


{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

module Plots.API.Density
  (  densityPlot
   , densityPlot'
   , densityPlotL
   , densityPlotOf
   , densityPlotOf'
   , densityPlotOfL
  ) where

import           Control.Lens                    hiding (( # ))
import           Control.Monad.State.Lazy
import           Data.Default
import           Data.Monoid.Recommend
import           Data.Typeable
import qualified Data.Foldable as F
import           Data.List
import           Data.Function

import           Diagrams.Coordinates.Isomorphic
import           Diagrams.Prelude
import           Diagrams.TwoD.Text
import           Linear

import           Plots.Axis
import           Plots.Axis.Grid
import           Plots.Axis.Labels
import           Plots.Axis.Render
import           Plots.Axis.Ticks
import           Plots.Axis.ColourBar

import           Plots.Types
import           Plots.Themes

import           Plots.Types.Density
import           Plots.API

------------------------------------------------------------------------
-- Density Plot
------------------------------------------------------------------------

densityPlot
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (DensityPlot v n) b,
      F.Foldable f ,
      Enum n, TypeableFloat n)
  => f p -> m ()
densityPlot d = addPlotable (mkDensityPlot d)

densityPlot'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (DensityPlot v n) b,
      F.Foldable f ,
      Enum n, TypeableFloat n)
  => f p -> PlotState (DensityPlot v n) b -> m ()
densityPlot' d = addPlotable' (mkDensityPlot d)

densityPlotL
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (DensityPlot v n) b,
      F.Foldable f ,
      Enum n, TypeableFloat n)
  => String -> f p  -> m ()
densityPlotL l d = addPlotableL l (mkDensityPlot d)

densityPlotOf
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (DensityPlot v n) b,
      Enum n, TypeableFloat n)
  => Fold s p -> s -> m ()
densityPlotOf f s = addPlotable (mkDensityPlotOf f s)

densityPlotOf'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (DensityPlot v n) b,
      Enum n, TypeableFloat n)
  => Fold s p -> s -> PlotState (DensityPlot v n) b -> m ()
densityPlotOf' f s = addPlotable' (mkDensityPlotOf f s)

densityPlotOfL
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (DensityPlot v n) b,
      Enum n, TypeableFloat n)
  => String -> Fold s p -> s -> m ()
densityPlotOfL l f s = addPlotableL l (mkDensityPlotOf f s)
