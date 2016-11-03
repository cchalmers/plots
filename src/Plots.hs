{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Plots
-- Copyright   :  (C) 2015 Christopher Chalmers
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Christopher Chalmers
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module defines types for axis labels and tick labels.
--
----------------------------------------------------------------------------
module Plots
  (

    -- | Axis definition (r2Axis and polarAxis), aspect ratio and scaling.
    module Plots.Axis

    -- | 'AxisStyle's are used to provide default colours and shapes
    -- for the plots of an axis.
  , module Plots.Style

    --------------------------------------------------------------------
    -- * Plot Types
    --------------------------------------------------------------------

    -- ** Scatter plot
    -- | Scatter plots display data as a collection of points. A scatter
    --   plot can also be configured to have a different style /
    --   transform depending on the data.

    -- | Scatter and bubble. Scatter and bubble plot api.
  , module Plots.Types.Scatter

    -- | Bar plots, individual or grouped.
  , module Plots.Types.Bar

    -- ** Line plot

    -- | Line, trail and path. Line plot, steps plot api & api for trail
    --   and path.
  , module Plots.Types.Line

    -- ** Heat map plot

    -- | 2D mapping from 'Double's to colours.
  , module Plots.Types.HeatMap

    -- ** Histogram plot

    -- | Histogram. API for histogram.
  , module Plots.Types.Histogram

    -- | Wedge and annular wedge. API for wedge, annular wegde and pie.
  , module Plots.Types.Pie

    --------------------------------------------------------------------
    -- * Low level
    --------------------------------------------------------------------

    -- | Definitions of bounds, axis scale, orientation, legend, generic
    --   plot ,plot spec and so on.
  , module Plots.Types

    -- | Grid lines and styles.
  , module Plots.Legend

    -- | Grid lines and styles.
  , module Plots.Axis.Grid

    -- | Axis labels and tick labels .
  , module Plots.Axis.Labels

    -- | Rendering system for polar and r2 axis.
  , module Plots.Axis.Render

    -- | The scaling/size options for an axis.
  , module Plots.Axis.Scale

    -- | Ticks properties and placement.
  , module Plots.Axis.Ticks

    -- | The plot title.
  , module Plots.Axis.Title

    -- | Colour bars.
  , module Plots.Axis.ColourBar

    -- | Polar coordinates
  , module Diagrams.Coordinates.Polar

  , (&=), (&~~)

  ) where

import Diagrams.Coordinates.Polar

import           Plots.Axis
import           Plots.Axis.ColourBar
import           Plots.Axis.Grid
import           Plots.Axis.Labels
import           Plots.Axis.Render
import           Plots.Axis.Scale
import           Plots.Axis.Title
import           Plots.Axis.Ticks

import           Plots.Legend
import           Plots.Style
import           Plots.Types
import           Plots.Util

import           Plots.Types.Bar
import           Plots.Types.HeatMap
import           Plots.Types.Histogram
import           Plots.Types.Line
import           Plots.Types.Pie
import           Plots.Types.Scatter

