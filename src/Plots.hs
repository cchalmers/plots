{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
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
  , renderAxis
  , r2Axis
  -- , r3Axis
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
    -- mydata :: [(Double, Double)]
    -- mydata = [(2,3), (2.3, 4.5), (3.1, 2.5), (3.8, 3.2)]
    -- @

    -- | Most of the time you can use functions like 'addScatterPlot' which add
    --   directly a plot directly to an axis. However, for more advanced use
    --   you may wish to work with the specific data types.
  , addPlotable

    -- ** Scatter plot
    -- | Put markers at points. For more options see 'Plots.Types.Scatter'
  , scatterPlot
  , mkScatterPlot
  -- , connectedScattered
  , module Plots.Types.Scatter

    -- ** Line plot
    -- | Plot simple lines.
  -- , addLinePlot
  -- , linePlot
  -- , multiLinePlot
  -- , linePlotFromPath
  , module Plots.Types.Line

  -- ** Parametric plot
  -- , parametricPlot

  -- ** Mesh plot
  -- , meshPlot
  -- , surfacePlot

    -- ** Function plot
    -- | Plot a given function.
  -- , addFunctionPlot
  -- , module Plots.Types.Function

    -- ** Bar plot
    -- | Bar plot
  -- , addBarPlot
  -- , barPlotAxis
  -- , module Plots.Types.Bar

    -- * Legend
  , addLegend
  -- , legendEntry

    -- * Themes
  , setTheme
  , coolTheme
  -- , corperateTheme
  -- , blackAndWhiteTheme

    -- * Diagrams essentials
  , (#)
  , Diagram, V2 (..), V3 (..)
  , SizeSpec
  , lc
  , fc

    -- * Optics
    -- ** Basis elements
    -- | These basis elements can be used to select a specific coordinate axis.
    --   These can be used henever a function has a @E v@ argument.
  , E (..), ex, ey, ez

    -- ** Common functions
    -- | These lens functions can be used to change some of the more advanced
    --   aspects of the axis.
  , (&)
  , set, (.~)
  , over, (%~)



    -- * Axis adjustments
    -- ** Axis size
  , xMin , xMax
  , yMin , yMax
  , zMin , zMax

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

import Control.Lens hiding (( # ))
import Diagrams.Prelude hiding (view)
import Diagrams.TwoD.Text
import Data.Typeable
import Data.Default
import Data.Foldable
import Diagrams.Coordinates.Isomorphic

import Plots.Types

import Plots.Axis.Labels
import Plots.Axis.Ticks
import Plots.Axis.Grid
import Plots.Axis.Render
import Plots.Axis

-- import Plots.Types.Bar
import Plots.Types.Scatter
-- import Plots.Types.Function
import Plots.Types.Line
-- import Plots.Types.Surface
import Plots.Themes

import Linear.V3

import Data.Monoid.Recommend

import Control.Monad.State.Lazy

type R2Backend b n = (Renderable (Path V2 n) b, Renderable (Text n) b, Typeable b, TypeableFloat n, Enum n)

-- | Standard 2D axis.
r2Axis :: R2Backend b n => Axis b V2 n
r2Axis = def

-- | Standard 2D axis.
-- r3Axis :: R2Backend b n => Axis b V3 n
-- r3Axis = def


-- Axis labels

-- | Label coördinate axes with x, y or x, y, z.
cartesianLabels :: Traversable v => Axis b v n -> Axis b v n
cartesianLabels = partsOf (axisLabels . traversed . axisLabelText)
                    .~ ["x", "y", "z"]

-- | Set the label for the given axis.
-- @
-- myaxis = 'r2Axis' # 'set' ('axisLabel' 'ex') "x-axis"
--        = 'r2Axis' & 'axisLabel' 'ex' .~ "x-axis"
--        = 'r2Axis' &~ 'axisLabel' 'ex' .= "x-axis"
-- @
axisLabel :: E v -> Lens' (Axis b v n) String
axisLabel (E e) = axisLabels . e . axisLabelText

-- | Set the position of the given axis label.
axisLabelPosition :: E v -> Lens' (Axis b v n) AxisLabelPosition
axisLabelPosition (E e) = axisLabels . e . axisLabelPos

-- | Set the position of all axes labels.
axesLabelPositions :: Traversable v =>
  Traversal' (Axis b v n) AxisLabelPosition
axesLabelPositions = axisLabels . traversed . axisLabelPos

-- | Set the gap between the axis and the axis label.
setAxisLabelGap :: E v -> Lens' (Axis b v n) n
setAxisLabelGap (E e) = axisLabels . e . axisLabelGap

-- | Set the gaps between all axes and the axis labels.
setAxesLabelGaps :: Traversable v => Traversal' (Axis b v n) n
setAxesLabelGaps = axisLabels . traversed . axisLabelGap

-- | Add something 'Plotable' to an 'Axis'.
-- @
-- myaxis = r2Axis # addPlotable (mkScatterPlot mydata)
-- @
addPlotable :: (Plotable a b, Typeable (N a), Typeable b, Typeable (V a))
            => PlotProperties b (V a) (N a) -> a -> Axis b (V a) (N a) -> Axis b (V a) (N a)
addPlotable pp p = axisPlots <>~ [review _Plot (p, pp)]

-- Scatter plot

-- | Add a scatter plot from a foldable container of something
--   'PointLike' (i.e. (P2, (n, n))). So for PointLike V2 n a we could
--   have
--   @@
--   f a :: [(n, n)]
--   f a :: Vector (V2 n)
--   @@
scatterPlot
  :: (Plotable (ScatterPlot v n) b, PointLike v n a, Foldable f, R2Backend b n)
  => PlotProperties b v n -> f a -> Axis b v n -> Axis b v n
scatterPlot pp = addPlotable pp . mkScatterPlot

-- Line plot

-- Add a line plot from a foldable container of something 'PointLike'. For an V2 n Axis this could be
-- @@
-- f a :: Vector (n,n)
-- f a :: [P2]
-- @@
-- linePlot
--   :: (PointLike v n a, R2Backend b n, Foldable f)
--     => f a -> Axis b v n -> Axis b v n
-- linePlot = addPlotable . mkLinePlotFromVerticies

-- Add a multi-line plot from a foldable container of a foldable container of
-- something 'PointLike'. For an V2 n Axis this could be
-- @@
-- g (f a) :: [Vector (n,n)]
-- g (f a) :: V4 [P2]
-- @@
-- multiLinePlot
--   :: (PointLike v n a, R2Backend b n, Foldable f, Foldable g)
--     => g (f a) -> Axis b v n -> Axis b v n
-- multiLinePlot = addPlotable . mkMultiLinePlotFromVerticies

-- linePlotFromPath
--   :: (Plotable (LinePlot b v n), R2Backend b n, Euclidean v)
--     => Path v n -> Axis b v n -> Axis b v n
-- linePlotFromPath = addPlotable . mkLinePlotFromPath

-- Parametric plots

-- parametricPlot
--   :: (PointLike v n a, R2Backend b n, Applicative v, Metric v, Typeable v)
--     => (n -> a) -> Axis b v n -> Axis b v n
-- parametricPlot = addPlotable . mkParametricPlot

-- Mesh plots

-- meshPlot :: R2Backend b n => (n -> n -> n) -> Axis b V3 n -> Axis b V3 n
-- meshPlot = addPlotable . mkMeshPlot


-- Surface plot

-- surfacePlot :: R2Backend b n => (n -> n -> n) -> Axis b V3 n -> Axis b V3 n
-- surfacePlot = addPlotable . mkSurfacePlot


-- Legend

addLegend :: (HasPlotProperties a, Num (N a)) => String -> a -> a
addLegend txt = legendEntries <>~ pure (mkLegendEntry txt)

-- legendEntry :: (Plotable a, Num (N a)) => String -> a -> a
-- legendEntry txt = legendEntries <>~ pure (mkLegendEntry txt)



-- | Set the aspect ratio of given axis.
setAxisRatio :: E v -> n -> Axis b v n -> Axis b v n
setAxisRatio e = set (axisScaling . el e . aspectRatio) . Commit

-- | Make each axis have the same unit length.
equalAxis :: (Traversable v, Num n) => Axis b v n -> Axis b v n
equalAxis = axisScaling . traversed . aspectRatio .~ Commit 1

-- Themes

setTheme :: Theme b n -> Axis b v n -> Axis b v n
setTheme = set axisTheme


-- Grid lines

-- | Set no major or minor grid lines for all axes.
noGridLines :: Traversable v => Axis b v n -> Axis b v n
noGridLines = noMajorGridLines . noMinorGridLines

-- Majors

-- | Add major grid lines for all axes.
addMajorGridLines :: Traversable v => Axis b v n -> Axis b v n
addMajorGridLines = set (axisGridLines . traversed . majorGridF) tickGridF

-- | Add major grid lines for given axis.
addMajorGridLine :: E v -> Axis b v n -> Axis b v n
addMajorGridLine (E e) = set (axisGridLines . e . majorGridF) tickGridF

-- | Set no major grid lines for all axes.
noMajorGridLines :: Traversable v => Axis b v n -> Axis b v n
noMajorGridLines = set (axisGridLines . traversed . majorGridF) noGridF

-- | Set no major grid lines for given axis.
noMajorGridLine :: E v -> Axis b v n -> Axis b v n
noMajorGridLine (E e) = set (axisGridLines . e . majorGridF) noGridF

-- Minors

-- | Add minor grid lines for all axes.
addMinorGridLines :: Traversable v => Axis b v n -> Axis b v n
addMinorGridLines = set (axisGridLines . traversed . minorGridF) tickGridF

-- | Add minor grid lines for given axis.
addMinorGridLine :: E v -> Axis b v n -> Axis b v n
addMinorGridLine (E e) = set (axisGridLines . e . minorGridF) tickGridF

-- | Set no minor grid lines for all axes.
noMinorGridLines :: Traversable v => Axis b v n -> Axis b v n
noMinorGridLines = set (axisGridLines . traversed . minorGridF) noGridF

-- | Set no minor grid lines for given axis.
noMinorGridLine :: E v -> Axis b v n -> Axis b v n
noMinorGridLine (E e) = set (axisGridLines . e . minorGridF) noGridF

xMax :: (HasBounds a, R1 (V a)) => Lens' a (Recommend (N a))
xMax = bounds . _Wrapped . _x . upperBound

xMin :: (HasBounds a, R1 (V a)) => Lens' a (Recommend (N a))
xMin = bounds . _Wrapped . _x . lowerBound

yMax :: (HasBounds a, R2 (V a)) => Lens' a (Recommend (N a))
yMax = bounds . _Wrapped . _y . upperBound

yMin :: (HasBounds a, R2 (V a)) => Lens' a (Recommend (N a))
yMin = bounds . _Wrapped . _y . lowerBound

zMax :: (HasBounds a, R3 (V a)) => Lens' a (Recommend (N a))
zMax = bounds . _Wrapped . _z . upperBound

zMin :: (HasBounds a, R3 (V a)) => Lens' a (Recommend (N a))
zMin = bounds . _Wrapped . _z . lowerBound


{-# ANN module ("HLint: ignore Use import/export shortcut" :: String) #-}

-- -- | Traversal over all axis line types.
-- axisLineTypes :: HasAxisLines a v => Tranversal' a AxisLineType
-- axisLineTypes = axisLines . traversed . axisLine
--
-- -- | Lens onto x axis line type.
-- xAxisLineType :: (L.R1 v, HasAxisLines a v) => Lens' a AxisLineType
-- xAxisLineType = axisLine ex . axisLineType
--
-- -- | Lens onto y axis line type.
-- yAxisLineType :: (L.V2 n v, HasAxisLines a v) => Lens' a AxisLineType
-- yAxisLineType = axisLine ey . axisLineType
--
-- -- | Lens onto z axis line type.
-- zAxisLineType :: (L.V3 n v, HasAxisLines a v) => Lens' a AxisLineType
-- zAxisLineType = axisLine ez . axisLineType
--
-- xAxisArrowOpts :: (L.R1 v, HasAxisLines a v) => Lens' a (Maybe ArrowOpts)
-- xAxisArrowOpts = axisLine ex . axisArrowOpts
--
-- yAxisArrowOpts :: (L.V2 n v, HasAxisLines a v) => Lens' a (Maybe ArrowOpts)
-- yAxisArrowOpts = axisLine ey . axisArrowOpts
--
-- zAxisArrowOpts :: (L.V3 n v, HasAxisLines a v) => Lens' a (Maybe ArrowOpts)
-- zAxisArrowOpts = axisLines ez . axisArrowOpts
--
--
