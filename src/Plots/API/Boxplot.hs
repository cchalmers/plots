{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE AllowAmbiguousTypes          #-}


{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

module Plots.API.Boxplot
  (  boxPlot
   , boxPlot'
   , boxPlotL
   , boxPlotOf
   , boxPlotOf'
   , boxPlotOfL
  ) where

import           Control.Lens                    hiding (( # ))
import           Control.Monad.State.Lazy
import qualified Data.Foldable as F

import           Diagrams.Coordinates.Isomorphic
import           Diagrams.Prelude

import           Plots.Axis

import           Plots.Types

import           Plots.Types.Boxplot
import           Plots.API

------------------------------------------------------------------------
-- Box Plot
------------------------------------------------------------------

boxPlot
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (BoxPlot v n) b,
      F.Foldable f ,
      Enum n, TypeableFloat n)
  => f p -> m ()
boxPlot d = addPlotable (mkBoxPlot d)

boxPlot'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (BoxPlot v n) b,
      F.Foldable f ,
      Enum n, TypeableFloat n)
  => f p -> PlotState (BoxPlot v n) b -> m ()
boxPlot' d = addPlotable' (mkBoxPlot d)

boxPlotL
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (BoxPlot v n) b,
      F.Foldable f ,
      Enum n, TypeableFloat n)
  => String -> f p  -> m ()
boxPlotL l d = addPlotableL l (mkBoxPlot d)

boxPlotOf
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (BoxPlot v n) b,
      Enum n, TypeableFloat n)
  => Fold s p -> s -> m ()
boxPlotOf f s = addPlotable (mkBoxPlotOf f s)

boxPlotOf'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (BoxPlot v n) b,
      Enum n, TypeableFloat n)
  => Fold s p -> s -> PlotState (BoxPlot v n) b -> m ()
boxPlotOf' f s = addPlotable' (mkBoxPlotOf f s)

boxPlotOfL
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (BoxPlot v n) b,
      Enum n, TypeableFloat n)
  => String -> Fold s p -> s -> m ()
boxPlotOfL l f s = addPlotableL l (mkBoxPlotOf f s)

