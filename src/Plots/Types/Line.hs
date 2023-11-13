{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE FunctionalDependencies    #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Plots.Types.Line
-- Copyright   :  (C) 2016 Christopher Chalmers
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Christopher Chalmers
-- Stability   :  experimental
-- Portability :  non-portable
--
-- A line plot is simply a 'Path' used as a plot. This module contains
-- helpers adding path plots. For line plots with markers, see
-- 'Plots.Types.Scatter'.
--
----------------------------------------------------------------------------

module Plots.Types.Line
  ( -- Plot trails/paths
    trailPlot
  , trailPlot'
  , pathPlot
  , pathPlot'

    -- * Line plots from points
  , linePlot
  , linePlot'
  , smoothLinePlot
  , smoothLinePlot'

    -- * Construction utilities

    -- ** Trails
  , mkTrail
  , mkTrailOf

    -- ** Paths
  , mkPath
  , mkPathOf


  ) where

import           Control.Monad.State.Lazy

import qualified Data.Foldable    as F

import           Diagrams.Coordinates.Isomorphic
import           Diagrams.Prelude

import           Plots.Axis
import           Plots.Types

------------------------------------------------------------------------
-- Trails and Paths
------------------------------------------------------------------------

-- | Add a 'Trail' as a 'Plot' to an 'Axis'.
trailPlot
  :: (BaseSpace c ~ v,
      Plotable (Path v n) b,
      MonadState (Axis b c n) m)
  => Trail v n -- ^ trail to plot
  -> State (Plot (Path v n) b) () -- ^ changes to plot options
  -> m () -- ^ add plot to the 'Axis'
trailPlot = pathPlot . toPath

-- | Add a 'Trail' as a 'Plot' to an 'Axis' without changes to the plot
--   options.
trailPlot'
  :: (BaseSpace c ~ v,
      Plotable (Path v n) b,
      MonadState (Axis b c n) m)
  => Trail v n -- ^ trail to plot
  -> m () -- ^ add plot to the 'Axis'
trailPlot' = pathPlot' . toPath

-- | Add a 'Path' as a 'Plot' to an 'Axis'.
pathPlot
  :: (BaseSpace c ~ v,
      Plotable (Path v n) b,
      MonadState (Axis b c n) m)
  => Path v n -- ^ path to plot
  -> State (Plot (Path v n) b) () -- ^ changes to plot options
  -> m () -- ^ add plot to the 'Axis'
pathPlot = addPlotable

-- | Add a 'Path' as a 'Plot' to an 'Axis' without changes to the plot
--   options.
pathPlot'
  :: (BaseSpace c ~ v,
      Plotable (Path v n) b,
      MonadState (Axis b c n) m)
  => Path v n -- ^ path to plot
  -> m () -- ^ add plot to the 'Axis'
pathPlot' = addPlotable'

------------------------------------------------------------------------
-- From list of points
------------------------------------------------------------------------

-- | Add a 'Path' plot from a list of points.
linePlot
  :: (BaseSpace c ~ v,
      Metric v,
      F.Foldable f,
      PointLike v n p,
      Plotable (Path v n) b,
      MonadState (Axis b c n) m)
  => f p -- ^ points to turn into trail
  -> State (Plot (Path v n) b) () -- ^ changes to plot options
  -> m () -- ^ add plot to the 'Axis'
linePlot = addPlotable . toPath . mkTrail

-- | Add a 'Path' plot from a list of points.
linePlot'
  :: (BaseSpace c ~ v,
      Metric v,
      F.Foldable f,
      PointLike v n p,
      Plotable (Path v n) b,
      MonadState (Axis b c n) m)
  => f p -- ^ points to turn into trail
  -> m () -- ^ add plot to the 'Axis'
linePlot' = addPlotable' . toPath . mkTrail

-- | Add a smooth 'Path' plot from a list of points using 'cubicSpline'.
smoothLinePlot
  :: (BaseSpace c ~ v,
      F.Foldable f,
      Metric v,
      PointLike v n p,
      Plotable (Path v n) b,
      Fractional (v n), -- needs fixing in diagrams-lib
      MonadState (Axis b c n) m)
  => f p -- ^ points to turn into trail
  -> State (Plot (Path v n) b) () -- ^ changes to plot options
  -> m () -- ^ add plot to the 'Axis'
smoothLinePlot = addPlotable . cubicSpline False . toListOf (folded . unpointLike)

-- | Add a smooth 'Path' plot from a list of points using 'cubicSpline'
--   without changes to the plot options.
smoothLinePlot'
  :: (BaseSpace c ~ v,
      F.Foldable f,
      PointLike v n p,
      Plotable (Path v n) b,
      Fractional (v n), -- needs fixing in diagrams-lib
      MonadState (Axis b c n) m)
  => f p -- ^ points to turn into trail
  -> m () -- ^ add plot to the 'Axis'
smoothLinePlot' xs = smoothLinePlot xs (return ())

------------------------------------------------------------------------
-- Trail and path
------------------------------------------------------------------------

-- | Construct a localed trail from a list of foldable of points.
mkTrail :: (PointLike v n p, OrderedField n, F.Foldable f) => f p -> Located (Trail v n)
mkTrail = mkTrailOf folded

-- | Construct a localed trail from a fold over points.
mkTrailOf :: (PointLike v n p, OrderedField n) => Fold s p -> s -> Located (Trail v n)
mkTrailOf f ps = fromVertices $ toListOf (f . unpointLike) ps

-- | Construct a localed trail from a fold over points.
mkPath :: (PointLike v n p, OrderedField n, F.Foldable f, F.Foldable g) => g (f p) -> Path v n
mkPath pss = toPath $ map mkTrail (F.toList pss)

-- | Construct a localed trail from a fold over points.
mkPathOf :: (PointLike v n p, OrderedField n) => Fold s t -> Fold t p -> s -> Path v n
mkPathOf f1 f2 as = Path $ map (mkTrailOf f2) (toListOf f1 as)

