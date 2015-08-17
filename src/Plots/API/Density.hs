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
import qualified Data.Foldable as F

import           Diagrams.Coordinates.Isomorphic
import           Diagrams.Prelude

import           Plots.Axis

import           Plots.Types

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
