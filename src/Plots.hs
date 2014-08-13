{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Plots
-- Copyright   :  (c) 2014 Christopher Chalmers
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  c.chalmers@me.com
--
-- This module should contain everything you need to get started with @plots@. 
-- Other modules are exposed for advanced customisation, manly with the use of 
-- lenses.
-- 
-- The aim of this library is to easily create good looking plots with the 
-- ability to customise almost every aspect when needed.
-- 
-- Plots is based on the "Diagrams" library and adopts many of its 
-- styles/convections. All plots are converted to diagrams and diagrams can be 
-- included in plots. See the Diagrams library for more info.
-- 
-- The @Axis b@ type holds all the information necessary to build a plot. The 
-- simplest way to draw a plot is to start with one of the default axis and add 
-- some plots to it.
-- 
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- NOTE: This doesn't actually work yet!
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- 
-- @@
-- import Plots
-- import Diagrams.Backend.PGF -- insert favourite backend here
-- 
-- myPlot :: Axis B
-- myPlot = defaultAxis
--            & addFunctionPlot sin
--                # withLegendEntry "sine wave"
--            & addFunctionPlot cos
--                # withLegendEntry "cosine wave"
--            & set xMax 6
--
-- myPlot = defaultAxis
--            & addPlots
--                [ functionPlot sin
--                    & addLegendEntry "sine wave"
--                , functionPlot cos
--                    & addLegendEntry "cosine wave"
--                ]
--            & set xMax 6
--
-- myPlot = defaultAxis &~ do
--   axisPlots <>= functionPlot sin
--     # addLegendEntry "sine wave"
--   axisPlots <>= functionPlot cos
--     # addLegendEntry "cosine wave"
-- 
-- 
-- main = defaultMain myPlot
-- @@
--
-- @@
-- $ runHaskell myPlot.hs -o myPlot.pdf
-- @@
-- 
-- 
-- Currently @plots@ supports line, scatter, function and bar plots. Hopefully 
-- more will be added soon.
-- 
--
-----------------------------------------------------------------------------
module Plots
  ( -- * Axis
    Axis
  , r2Axis
  , renderR2Axis
  , r3Axis
  , renderR3Axis
  -- , logAxis
  -- , module Plots.Axis

    -- * Plots
  , Plot (..)

    -- ** Adding plots
    -- | Most plotting functions are highly polymorphic to allow a wide range 
    --   of inputs. One downside of this is the type system cannot always infer 
    --   what type the input should be. It is therefore a good idea to give 
    --   type signatures to input data.
    -- 
    -- @
    -- mydata :: [(Double,Double)]
    -- mydata = [(2,3), (2.3, 4.5), (3.1, 2.5), (3.8, 3.2)]
    -- @

    -- | Most of the time you can use functions like 'addScatterPlot' which add 
    --   directly a plot directly to an axis. However, for more advanced use 
    --   you may wish to work with the specific data types.
  , addPlotable

    -- ** Scatter plot
    -- | Put markers at points. For more options see 'Plots.Types.Scatter'
  -- , addScatterPlot
  , addScatterPlotable
  -- , connectedScattered
  , module Plots.Types.Scatter

    -- ** Line plot
    -- | Plot simple lines.
  -- , addLinePlot
  -- , addMultiLinePlot
  , module Plots.Types.Line

    -- ** Function plot
    -- | Plot a given function.
  -- , addFunctionPlot
  , module Plots.Types.Function

    -- ** Bar plot
    -- | Bar plot
  -- , addBarPlot
  -- , barPlotAxis
  , module Plots.Types.Bar


    -- * Themes
  , setTheme
  , coolTheme
  -- , corperateTheme
  -- , blackAndWhiteTheme

    -- * Diagrams essentials
  , (#)
  , Diagram , R2, R3
  , SizeSpec2D (..)
  , lc
  , fc

    -- * Optics
    -- ** Basis elements
    -- | These basis elements can be used to select a specific coordinate axis. 
    --   These can be used henever a function has a @E (T v)@ argument.
  , E (..), ex, ey, ez

    -- ** Common functions
    -- | These lens functions can be used to change some of the more advanced 
    --   aspects of the axis.
  , (&)
  , set, (.~)
  , over, (%~)



    -- * Axis adjustments
    -- ** Axis size
  -- , xMin , xMax
  -- , yMin , yMax
  -- , zMin , zMax

   -- ** Axis labels
   -- *** Label text
  , axisLabel
  , cartesianLabels

    -- *** Label position
  , AxisLabelPosition (..)
  , axesLabelPositions
  , axisLabelPosition

    -- *** Label gaps
  , setAxisLabelGap
  , setAxesLabelGaps

    -- * Axis scaling
  , setAxisRatio
  , equalAxis

    -- ** Axis line type
  , AxisLineType (..)
  -- , allAxisLineType
  -- , xAxisLineType
  -- , yAxisLineType
  -- , zAxisLineType

    -- * Axis ticks
    -- | Placement of major and minor ticks.
  -- , noMinorTicks
  -- , noMajorTicks
  , module Plots.Axis.Ticks

    -- * Axis Tick Labels
  , module Plots.Axis.Labels

    -- ** Axis Grid
  , noGridLines
  , addMajorGridLines, addMajorGridLine
  , noMajorGridLines, noMajorGridLine
  , addMinorGridLines, addMinorGridLine
  , noMinorGridLines, noMinorGridLine

    -- * do notation
    -- | If you prefer you can use do notation with the @(&~)@ operator.
    -- ** State operators
    -- ```
    -- myaxis = r2axis &~ do
    --   axisLabel ex .= "x-axis"
    --   assign axesLabelPositions LowerAxisLabel
    --   modify noGridLines
    -- ```
  , (&~)
  , (.=), assign
  , (%=), modify
  ) where

import Diagrams.Prelude2

import Plots.Types

import Plots.Axis.Labels
import Plots.Axis.Ticks
import Plots.Axis.Grid
import Plots.Axis

import Plots.Types.Bar
import Plots.Types.Scatter
import Plots.Types.Function
import Plots.Types.Line
import Plots.Themes

import Data.Monoid.Recommend

import Control.Monad.State.Lazy

import Diagrams.Coordinates.Isomorphic




-- | Standard 2D axis.
r2Axis :: (Renderable (Path R2) b, Renderable Text b) => Axis b R2
r2Axis = def

-- | Standard 2D axis.
r3Axis :: (Renderable (Path R2) b, Renderable Text b) => Axis b R3
r3Axis = def


-- Axis labels

-- | Label coördinate axes with x, y or x, y, z.
cartesianLabels :: Traversable (T v) => Axis b v -> Axis b v
cartesianLabels = partsOf (axisLabels . traversed . axisLabelText)
               .~ ["x", "y", "z"]

-- | Set the label for the given axis.
-- @
-- myaxis = 'r2Axis' # 'set' ('axisLabel' 'ex') "x-axis"
--        = 'r2Axis' & 'axisLabel' 'ex' .~ "x-axis"
--        = 'r2Axis' &~ 'axisLabel' 'ex' .= "x-axis"
-- @
axisLabel :: E (T v) -> Lens' (Axis b v) String
axisLabel (E e) = axisLabels . e . axisLabelText

-- | Set the position of the given axis label.
axisLabelPosition :: E (T v) -> Lens' (Axis b v) AxisLabelPosition
axisLabelPosition (E e) = axisLabels . e . axisLabelPos

-- | Set the position of all axes labels.
axesLabelPositions :: Traversable (T v) =>
  Traversal' (Axis b v) AxisLabelPosition
axesLabelPositions = axisLabels . traversed . axisLabelPos

-- | Set the gap between the axis and the axis label.
setAxisLabelGap :: E (T v) -> Lens' (Axis b v) Double
setAxisLabelGap (E e) = axisLabels . e . axisLabelGap

-- | Set the gaps between all axes and the axis labels.
setAxesLabelGaps :: Traversable (T v) => Traversal' (Axis b v) Double
setAxesLabelGaps = axisLabels . traversed . axisLabelGap

-- | Add something 'Plotable' to an 'Axis'.
-- @
-- myaxis = r2Axis # addPlotable (mkScatterPlot mydata)
-- @
addPlotable :: Plotable a b v => a -> Axis b v -> Axis b v
addPlotable p = axisPlots <>~ [review _Plot p]

-- Scatter plot

-- | Add a scatter plot from a foldable container of something 
--   'PointLike' (i.e. (P2, (Double, Double))). So for PointLike a R2 we could 
--   have
--   @@
--   f a :: [(Double, Double)]
--   f a :: Vector (V2 Double)
--   @@
addScatterPlotable
  :: (PointLike a v,
      Foldable f,
      Default (ScatterPlot b v),

      -- HasGenericPlot a b v,
      Scalar v ~ Double,
      Renderable (Path R2) b,
      Typeable b)
      -- Plotable (ScatterPlot b v) b v)
  => f a -> Axis b v -> Axis b v
addScatterPlotable = addPlotable . mkScatterPlot

-- class (HasGenericPlot a b v, Typeable a, Typeable b) => Plotable a b v where
--   plot :: T v (Double, Double) -> (v :-* R2) -> T2 -> a -> Diagram b R2

-- -- | Traversal over all axis line types.
-- axisLineTypes :: HasAxisLines a v => Tranversal' a AxisLineType
-- axisLineTypes = axisLines . traversed . axisLine
-- 
-- -- | Lens onto x axis line type.
-- xAxisLineType :: (L.R1 (T v), HasAxisLines a v) => Lens' a AxisLineType
-- xAxisLineType = axisLine ex . axisLineType
-- 
-- -- | Lens onto y axis line type.
-- yAxisLineType :: (L.R2 (T v), HasAxisLines a v) => Lens' a AxisLineType
-- yAxisLineType = axisLine ey . axisLineType
-- 
-- -- | Lens onto z axis line type.
-- zAxisLineType :: (L.R3 (T v), HasAxisLines a v) => Lens' a AxisLineType
-- zAxisLineType = axisLine ez . axisLineType
-- 
-- xAxisArrowOpts :: (L.R1 (T v), HasAxisLines a v) => Lens' a (Maybe ArrowOpts)
-- xAxisArrowOpts = axisLine ex . axisArrowOpts
-- 
-- yAxisArrowOpts :: (L.R2 (T v), HasAxisLines a v) => Lens' a (Maybe ArrowOpts)
-- yAxisArrowOpts = axisLine ey . axisArrowOpts
-- 
-- zAxisArrowOpts :: (L.R3 (T v), HasAxisLines a v) => Lens' a (Maybe ArrowOpts)
-- zAxisArrowOpts = axisLines ez . axisArrowOpts
-- 
--

-- | Set the aspect ratio of given axis.
setAxisRatio :: E (T v) -> Double -> Axis b v -> Axis b v
setAxisRatio e = set (axisScaling . el e . aspectRatio) . Commit

-- | Make each axis have the same unit length.
equalAxis :: Traversable (T v) => Axis b v -> Axis b v
equalAxis = axisScaling . traversed . aspectRatio .~ Commit 1


-- Themes

setTheme :: Theme b -> Axis b v -> Axis b v
setTheme = set axisTheme


-- Grid lines

-- | Set no major or minor grid lines for all axes.
noGridLines :: Traversable (T v) => Axis b v -> Axis b v
noGridLines = noMajorGridLines . noMinorGridLines

-- Majors

-- | Add major grid lines for all axes.
addMajorGridLines :: Traversable (T v) => Axis b v -> Axis b v
addMajorGridLines = set (axisGridLines . traversed . majorGridF) tickGridF

-- | Add major grid lines for given axis.
addMajorGridLine :: E (T v) -> Axis b v -> Axis b v
addMajorGridLine (E e) = set (axisGridLines . e . majorGridF) tickGridF

-- | Set no major grid lines for all axes.
noMajorGridLines :: Traversable (T v) => Axis b v -> Axis b v
noMajorGridLines = set (axisGridLines . traversed . majorGridF) noGridF

-- | Set no major grid lines for given axis.
noMajorGridLine :: E (T v) -> Axis b v -> Axis b v
noMajorGridLine (E e) = set (axisGridLines . e . majorGridF) noGridF

-- Minors

-- | Add minor grid lines for all axes.
addMinorGridLines :: Traversable (T v) => Axis b v -> Axis b v
addMinorGridLines = set (axisGridLines . traversed . minorGridF) tickGridF

-- | Add minor grid lines for given axis.
addMinorGridLine :: E (T v) -> Axis b v -> Axis b v
addMinorGridLine (E e) = set (axisGridLines . e . minorGridF) tickGridF

-- | Set no minor grid lines for all axes.
noMinorGridLines :: Traversable (T v) => Axis b v -> Axis b v
noMinorGridLines = set (axisGridLines . traversed . minorGridF) noGridF

-- | Set no minor grid lines for given axis.
noMinorGridLine :: E (T v) -> Axis b v -> Axis b v
noMinorGridLine (E e) = set (axisGridLines . e . minorGridF) noGridF



{-# ANN module ("HLint: ignore Use import/export shortcut" :: String) #-}

