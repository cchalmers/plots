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
    -- * API

    -- | Data types and classes, definitions of plotable, bounds, axis
    --   labels, legends, diagram essentials and so on.
    module Plots.API

    -- | 'AxisStyle's are used to provide default colours and shapes
    -- for the plots of an axis.
  , module Plots.Style

    -- | Generate a command line tool to make a plot.
  , module Plots.CmdLine

    --------------------------------------------------------------------
    -- * Plot Types
    --------------------------------------------------------------------

    -- ** Scatter plot
    -- | Scatter plots display data as a collection of points. A scatter
    --   plot can also be configured to have a different style /
    --   transform depending on the data.

  , scatterPlot
  , bubblePlot
  , gscatterPlot

    -- | Scatter and bubble. Scatter and bubble plot api.
  , module Plots.Types.Scatter

    -- ** Line plot
    -- | Line plots display 'Path's as a plot.

  , linePlot

    -- | Line, trail and path. Line plot, steps plot api & api for trail
    --   and path.
  , module Plots.Types.Line

    -- ** Ribbon plot

    -- | Ribbon and area. Ribbon and area plot api.
  , module Plots.Types.Ribbon

    -- ** Histogram plot

    -- | Histogram. API for histogram.
  , module Plots.Types.Histogram

    -- ** Function plot

    -- | Parametric functions and vectors. Parametric plot and vectors
    --   api.
  , module Plots.Types.Function

    -- ** Box plot

    -- | Boxplot. Boxplot api.
  , module Plots.Types.Boxplot

    -- ** Density function

    -- | Density function. Density plot api.
  , module Plots.Types.Density

    -- ** Smooth function

    -- | Smooth. Smooth plot api.
  , module Plots.Types.Smooth

    -- | Text. API for text plot.
  , module Plots.Types.Text

    -- | Wedge and annular wedge. API for wedge, annular wegde and pie.
  , module Plots.Types.Pie

    -- | Scatter plot for polar co-ordinates. API for polar scatter
    --   plot.
  , module Plots.Types.Points

    -- | API using multiple types.
  , module Plots.Types.Others

    --------------------------------------------------------------------
    -- * Low level
    --------------------------------------------------------------------

    -- | Definitions of bounds, axis scale, orientation, legend, generic
    --   plot ,plot spec and so on.
  , module Plots.Types

    -- | Axis definition (r2Axis and polarAxis), aspect ratio and scaling.
  , module Plots.Axis

    -- | Grid lines and styles.
  , module Plots.Axis.Grid

    -- | Axis labels and tick labels .
  , module Plots.Axis.Labels

    -- | Rendering system for polar and r2 axis, and system for
    --   calculating bounds and scale.
  , module Plots.Axis.Render

    -- | Ticks properties and placement.
  , module Plots.Axis.Ticks

    -- | Colour bars.
  , module Plots.Axis.ColourBar

  ) where

import           Plots.Axis
import           Plots.Axis.Grid
import           Plots.Axis.Labels
import           Plots.Axis.Render
import           Plots.Axis.Ticks
import           Plots.Axis.ColourBar

import           Plots.Types
import           Plots.Style
import           Plots.API
import           Plots.CmdLine

import           Plots.Types.Scatter
import           Plots.Types.Line
import           Plots.Types.Ribbon
import           Plots.Types.Histogram
import           Plots.Types.Function
import           Plots.Types.Boxplot
import           Plots.Types.Density
import           Plots.Types.Smooth
import           Plots.Types.Text
import           Plots.Types.Pie
import           Plots.Types.Points
import           Plots.Types.Others

