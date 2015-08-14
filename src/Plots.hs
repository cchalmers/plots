module Plots
  ( 
    -- * Core Library

    -- | Definitions of bounds, axis scale, orientation,
    -- legend, generic plot ,plot spec and so on.
    module Plots.Types

    -- | Definitions of theme, plots style, common themes,
    -- marker shapes, colour maps and sample maps.
  , module Plots.Themes

    -- * Axis Library

    -- | Axis definition (r2Axis & polarAxis), aspect
    -- ratio and scaling.
  , module Plots.Axis

    -- | Grid lines and styles.
  , module Plots.Axis.Grid

    -- | Axis labels and tick labels .
  , module Plots.Axis.Labels

    -- | Rendering system for polar and r2 axis, and system
    -- for calculating bounds and scale.
  , module Plots.Axis.Render

    -- | Ticks properties and placement.
  , module Plots.Axis.Ticks
   
    -- | Colour bars.
  , module Plots.Axis.ColourBar

    -- * Plot Types

    -- | Scatter and bubble.
  , module Plots.Types.Scatter
    
    -- | Line, trail and path.
  , module Plots.Types.Line

    -- | Ribbon and area.
  , module Plots.Types.Ribbon
  
    -- | Histogram.
  , module Plots.Types.Histogram

    -- | Parametric functions and vectors.
  , module Plots.Types.Function

    -- | Boxplot.
  , module Plots.Types.Boxplot

    -- | Density fucntion.
  , module Plots.Types.Density

    -- | Smooth.
  , module Plots.Types.Smooth

    -- | Text.
  , module Plots.Types.Text

    -- | Wedge and annular wedge.
  , module Plots.Types.Pie

    -- | Scatter plot for polar co-ordinates.
  , module Plots.Types.Points

  -- , module Plots.Types.Heatmap

    -- * API
    
    -- | Data types and classes, definitions of
    -- plotable, bounds, axis labels, legends, 
    -- diagram essentials and so on.
  , module Plots.API

    -- | Scatter and bubble plot api.
  , module Plots.API.Scatter

    -- | Line plot, steps plot api & api for trail and path.
  , module Plots.API.Line

    -- | Ribbon and area plot api.
  , module Plots.API.Ribbon

    -- | API for histogram.
  , module Plots.API.Histogram

    -- | Parametric plot and vectors api.
  , module Plots.API.Function

    -- | Boxplot api.
  , module Plots.API.Boxplot

    -- | Density plot api.
  , module Plots.API.Density

    -- | Smooth plot api.
  , module Plots.API.Smooth

    -- | API for text plot.
  , module Plots.API.Text

    -- | API using multiple types.
  , module Plots.API.Others

  -- , module Plots.API.Pie
  -- , module Plots.API.Points
  -- , module Plots.API.Heatmap
  ) where

import           Plots.Axis
import           Plots.Axis.Grid
import           Plots.Axis.Labels
import           Plots.Axis.Render
import           Plots.Axis.Ticks
import           Plots.Axis.ColourBar

import           Plots.Types
import           Plots.Themes

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
-- import          Plots.Types.Heatmap

import           Plots.API
import           Plots.API.Scatter
import           Plots.API.Line
import           Plots.API.Ribbon
import           Plots.API.Histogram
import           Plots.API.Function
import           Plots.API.Boxplot
import           Plots.API.Density
import           Plots.API.Smooth
import           Plots.API.Others
import           Plots.API.Text
-- import           Plots.API.Heatmap
-- import           Plots.API.Pie
-- import           Plots.API.Points

