module Plots
  ( -- * Core Library

    -- | Definitions of bounds, axis scale, orientation,
    -- legend, generic plot ,plot spec and so on.
    module Plots.Types

    -- | Definitions of theme, plots style, common themes,
    -- marker shapes, colour maps and sample maps.
  , module Plots.Themes

    -- * API
    
    -- | Data types and classes, definitions of
    -- plotable, bounds, axis labels, legends, 
    -- diagram essentials and so on.
  , module Plots.API

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
    -- Scatter and bubble plot api.
  , module Plots.Types.Scatter
    
    -- | Line, trail and path.
    -- Line plot, steps plot api & api for trail and path.
  , module Plots.Types.Line

    -- | Ribbon and area.
    -- Ribbon and area plot api.
  , module Plots.Types.Ribbon
  
    -- | Histogram.
    -- API for histogram.
  , module Plots.Types.Histogram

    -- | Parametric functions and vectors.
    -- Parametric plot and vectors api.
  , module Plots.Types.Function

    -- | Boxplot.
    -- Boxplot api.
  , module Plots.Types.Boxplot

    -- | Density fucntion.
    -- Density plot api.
  , module Plots.Types.Density

    -- | Smooth.
    -- Smooth plot api.
  , module Plots.Types.Smooth

    -- | Text.
    -- API for text plot.
  , module Plots.Types.Text

    -- | Wedge and annular wedge.
    -- API for wedge, annular wegde and pie.
  , module Plots.Types.Pie

    -- | Scatter plot for polar co-ordinates.
    -- API for polar scatter plot.
  , module Plots.Types.Points

    -- | API using multiple types.
  , module Plots.Types.Others

  -- , module Plots.Types.Heatmap
  ) where

import           Plots.Axis
import           Plots.Axis.Grid
import           Plots.Axis.Labels
import           Plots.Axis.Render
import           Plots.Axis.Ticks
import           Plots.Axis.ColourBar

import           Plots.Types
import           Plots.Themes
import           Plots.API

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
-- import          Plots.Types.Heatmap


