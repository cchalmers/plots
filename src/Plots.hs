{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Plots
-- Copyright   :  (c) 2015 Christopher Chalmers
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
-- The @Axis b v n@ type holds all the information necessary to build a plot. The
-- simplest way to draw a plot is to start with one of the default axis and add
-- some plots to it.
--
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- NOTE: This doesn't actually work yet!
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
--
-- @
-- import Plots
-- import Diagrams.Backend.PGF -- insert favourite backend here
--
-- myplot :: Axis B V2 Double
-- myplot = r2Axis &~ do
--   functionPlotL "sine wave" sin
--   functionPlotL "cosine wave" cos
--   xMax .= 6
--
-- main = axisMain myplot
-- @

module Plots
  ( AxisState
  , axisState
  , Axis
  , r2Axis
  , renderAxis
  , Plot (..)

    -- * Plotable
  , addPlotable
  , addPlotable'
  , addPlotableL

    -- ** Scatter plot
  , scatterPlot
  , scatterPlot'
  , scatterPlotL
  , scatterPlotOf
  , scatterPlotOf'
  , scatterPlotLOf

    -- ** Line plot
  , linePlot
  -- , linePlot'
  -- , linePlotL
  -- , linePlotL'

  , pathPlot
  -- , pathPlot'
  -- , pathPlotL
  -- , pathPlotL'

    -- ** Diagram plot
  , diagramPlot

    -- * Axis properties

    -- ** Bounds

  , noGridLines
  , addLegendEntry

    -- ** Ticks

  -- , ticks

    -- ** Grid lines

  -- , recentProps
  , cartesianLabels

    -- ** Axis labels
    -- ** Axis title

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
  , addLegendEntry

    -- * Themes
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
  -- , axisPlotProperties

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

import           Control.Lens                    hiding (( # ))
import           Control.Monad.State.Lazy
import           Data.Default
import           Data.Foldable
import           Data.Monoid.Recommend
import           Data.Typeable

import           Diagrams.Coordinates.Isomorphic
import           Diagrams.Prelude
import           Diagrams.TwoD.Text
import           Linear

import           Plots.Axis
import           Plots.Axis.Grid
import           Plots.Axis.Labels
import           Plots.Axis.Render
import           Plots.Axis.Ticks
import           Plots.Types
-- import           Plots.Themes

import           Plots.Types.Line
import           Plots.Types.Scatter
-- import Plots.Types.Bar
-- import Plots.Types.Function
-- import Plots.Types.Surface


-- $ state
-- This module uses the state monad to alter the 'Axis' and other
-- properties. This is mainly for nicer syntax. You may notice there are
-- many ways to achieve the same thing. This is because I'm
-- experimenting. There is a chance that some functions will change / be
-- removed. Let me know what you think!

-- $ pointlike
-- The 'PointLike' class is used for convienience so you don't have to
-- convert whatever data type you're already using. Some common
-- instances are:
--
-- @
-- PointLike V2 Double (Double,Double)
-- PointLike V2 Double (V2 Double)
-- PointLike V3 Float (Float, Float, Float)
-- @
--
-- This means whenever you see @PointLike v n p@ in a type constaint,
-- the @p@ in the type signature could be @(Double, Double)@ or @V2
-- Double@ or anything

-- $ data
-- Since the plot making functions are super-polymorphic it is
-- important the that data has a concrete type signature or the compiler
-- won't be able to work out which type to use. The following data is
-- used thoughout the examples:
--
-- @
-- pointData1 = [(1,3), (2,5.5), (3.2, 6), (3.5, 6.1)] :: [(Double,Double)]
-- pointData2 = U.fromList [V2 1.2 2.7, V2 2 5.1, V2 3.2 2.6, V2 3.5 5] :: U.Vector (V2 Double)
-- barData1 = [55.3, 43.2, 12.5, 18.3, 32.0] :: [Double]
-- barData2 = [("apples", 55), ("oranges",43), ("pears", 12), ("mangos", 18)] :: [(String, Double)]
-- @


type R2Backend b n = (Renderable (Path V2 n) b, Renderable (Text n) b, Typeable b, TypeableFloat n, Enum n)


-- newtype AxisStateM b v n a = AxisState (State (P.Axis b v n) a)
--   deriving (Functor, Applicative, Monad, MonadState (P.Axis b v n))

type AxisStateM b v n = State (Axis b v n)
type AxisState b v n  = AxisStateM b v n ()

type PlotStateM a b = State (PropertiedPlot a b)
type PlotState a b  = PlotStateM a b ()

-- type PropertyStateM b v n a = State (PlotProperties b v n) a
-- type PropertyState b v n = State (PlotProperties b v n) ()

-- newtype PlotStateM p b v n = PState (State (p, PlotProperties))
--   deriving (Functor, Applicative, Monad, MonadState (P.Axis b v n))

cartesianLabels :: Traversable v => AxisState b v n
cartesianLabels =
  partsOf (axisLabels . traversed . axisLabelText) .= ["x", "y", "z"]

------------------------------------------------------------------------
-- Plot properties
------------------------------------------------------------------------

-- $properties
-- Every plot has a assosiating 'PlotProperties'. These contain general
-- attributes like the legend entryies and the style and the name for
-- the plot.
--
-- There are several ways to adjust the properties.

-- $naming
-- The plot adding functions follow the following naming conventions:
--
-- * addPlot :: data -> AxisState
-- * addPlot' :: data -> PropertyState -> AxisState
-- * addPlotL :: String -> data -> PropertyState -> AxisState

------------------------------------------------------------------------
-- Plotable
------------------------------------------------------------------------

-- $plotable
-- The 'Plotable' class defines ways of converting the data type to a
-- diagram for some axis.

addPlotable :: (InSpace v n a, Plotable a b) => a -> AxisState b v n
addPlotable a = axisPlots %= flip snoc (Plot' a mempty)


-- | Add something 'Plotable' while adjusting the 'PlotProperties' for
--   that plot.
--
-- @
-- myaxis = r2Axis ~& do
--   addPlotable' (asPath $ square 3) $ do
--     plotName .= mkName 5
--     addLegendEntry "pentagon"
-- @
addPlotable' :: (InSpace v n a, Plotable a b)
             => a -> PlotStateM a b a0 -> AxisState b v n
addPlotable' a s = axisPlots <>= [Plot' a (Endo $ execState s)]

addPlotableL :: (InSpace v n a, Plotable a b)
             => String -> a -> AxisState b v n
addPlotableL l a = addPlotable' a $ addLegendEntry l

------------------------------------------------------------------------
-- Scatter plot
------------------------------------------------------------------------

-- $ scatter
-- Scatter plots display data as dots. There are several representations
-- for scatter plots for extra parameters. Scatter plots have the
-- following lenses:
--
-- @
-- * 'connectingLine' :: 'Lens'' ('ScatterPlot' v n) 'Bool' - False
-- * 'scatterTransform' :: 'Lens'' ('ScatterPlot' v n) ('Maybe' ('Point' v n -> 'T2' n)) - Nothing
-- * 'scatterStyle': 'Maybe' ('Point' v n -> 'Style' 'V2' n) - Nothing
-- @
--

-- | Add a 'ScatterPlot' to the 'AxisState' from a data set.
--
--   @
--   myaxis = r2Axis ~&
--     scatterPlot data1
--   @
scatterPlot :: (PointLike v n p, Plotable (ScatterPlot v n) b, Foldable f)
            => f p -> AxisState b v n
scatterPlot d = addPlotable (mkScatterPlot d)

-- | Make a 'ScatterPlot' and take a 'State' on the plot to alter it's
--   options
--
--   @
--   myaxis = r2Axis &~ do
--     scatterPlot' pointData1 $ do
--       connectingLine .= True
--       addLegendEntry "data 1"
--   @
scatterPlot' :: (PointLike v n p, Plotable (ScatterPlot v n) b, Foldable f)
             => f p -> PlotState (ScatterPlot v n) b -> AxisState b v n
scatterPlot' d = addPlotable' (mkScatterPlot d)

-- | Add a 'ScatterPlot' with the given name for the legend entry.
--
--   @
--   myaxis = r2Axis &~ do
--     scatterPlotL "data 1" pointData1
--   @
scatterPlotL :: (PointLike v n p, Plotable (ScatterPlot v n) b, Foldable f)
             => String -> f p -> AxisState b v n
scatterPlotL l d = addPlotableL l (mkScatterPlot d)

-- Fold varients

scatterPlotOf :: (PointLike v n p, Plotable (ScatterPlot v n) b) => Fold s p -> s -> AxisState b v n
scatterPlotOf f s = addPlotable (mkScatterPlotOf f s)

scatterPlotOf' :: (PointLike v n p, Plotable (ScatterPlot v n) b)
               => Fold s p -> s -> PlotState (ScatterPlot v n) b -> AxisState b v n
scatterPlotOf' f s = addPlotable' (mkScatterPlotOf f s)

scatterPlotLOf :: (PointLike v n p, Plotable (ScatterPlot v n) b)
               => String -> Fold s p -> s -> AxisState b v n
scatterPlotLOf l f s = addPlotableL l (mkScatterPlotOf f s)

------------------------------------------------------------------------
-- Bubble plot
------------------------------------------------------------------------

-- $ bubble
-- Scatter plots with extra numeric parameter. By default the extra
-- parameter is the scale of the marker but this can be changed. Bubble
-- plots admit the following lenses:
--
-- @
-- * 'connectingLine' :: 'Lens'' ('BubblePlot' v n) 'Bool' - @False@
-- * 'scatterTransform': ('Maybe' ('Point' v n -> 'T2' n)) - @Just (scaling . fst)@
-- * 'scatterStyle': 'Maybe' ('Point' v n -> 'Style' 'V2' n) - @Nothing@
-- @

-- bubblePlot :: (PointLike v n p, R2Backend b n, Plotable (P.ScatterPlot v n) b, Foldable f)
--             => f (n,p) -> AxisState b v n
-- bubblePlot d = axisPlots <>= [P.Plot (P.mkBubblePlot d) def]

-- bubblePlot' :: (PointLike v n p, R2Backend b n, Plotable (P.ScatterPlot v n) b, Foldable f)
--             => f (n,p) -> AxisState b v n
-- bubblePlot' d s = axisPlots <>= [P.Plot (execState s $ P.mkBubblePlot d) def]

------------------------------------------------------------------------
-- Line plot
------------------------------------------------------------------------

-- $line
-- Line plots are internally represented by 'Path'.

linePlot :: (PointLike v n p, R2Backend b n, Plotable (Path v n) b, Foldable f)
         => f p -> AxisState b v n
linePlot d = addPlotable (mkPath $ Identity d)

pathPlot :: R2Backend b n => Path V2 n -> AxisState b V2 n
pathPlot = addPlotable

------------------------------------------------------------------------
-- Bar Plot
------------------------------------------------------------------------

-- $bar
-- Bar plots are different in that you often want to specify the axis
-- name along with the data.

-- barPlot :: (R2Backend, Plotable (Path v n) b, Foldable f)
--         => f (String, n) -> AxisState b v n
-- barPlot d = addPlotable $ P.mkBarPlot' d

-- barPlot' :: (R2Backend, Plotable (Path v n) b, Foldable f)
--         => f (String, n) -> AxisState b v n
-- barPlot' d s = addPlotable (P.mkBarPlot d) s

-- barPlotL :: (R2Backend, Plotable (Path v n) b, Foldable f)
--         => String -> f (String, n) -> AxisState b v n
-- barPlotL s d = addPlotableL s (P.mkBarPlot d)

------------------------------------------------------------------------
-- Diagram Plot
------------------------------------------------------------------------

diagramPlot :: (Renderable (Path V2 n) b, Typeable b, Typeable v, Metric v, TypeableFloat n)
              => QDiagram b v n Any -> AxisState b v n
diagramPlot = addPlotable

------------------------------------------------------------------------
-- Axis properties
------------------------------------------------------------------------

-- | Traversal over the axis' most recent 'PlotProperties'.
-- recentProps :: PropertyState b v n -> AxisState b v n
-- recentProps s = axisPlots . _last . _2 %= (execState s .)

-- Legend
------------

addLegendEntry :: (HasPlotProperties a, MonadState a m, Num (N a))
               => String -> m ()
addLegendEntry s = legendEntries <>= [mkLegendEntry s]

axisState :: Axis b v n -> AxisStateM b v n a -> Axis b v n
axisState a s = execState s a


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

-- | Standard 2D axis.
r2Axis :: R2Backend b n => Axis b V2 n
r2Axis = def

-- | Standard 2D axis.
-- r3Axis :: R2Backend b n => Axis b V3 n
-- r3Axis = def



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
axesLabelPositions :: Traversable v => Traversal' (Axis b v n) AxisLabelPosition
axesLabelPositions = axisLabels . traversed . axisLabelPos

-- | Set the gap between the axis and the axis label.
setAxisLabelGap :: E v -> Lens' (Axis b v n) n
setAxisLabelGap (E e) = axisLabels . e . axisLabelGap

-- | Set the gaps between all axes and the axis labels.
setAxesLabelGaps :: Traversable v => Traversal' (Axis b v n) n
setAxesLabelGaps = axisLabels . traversed . axisLabelGap


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


-- | Set the aspect ratio of given axis.
setAxisRatio :: E v -> n -> AxisState b v n
setAxisRatio e n = axisScaling . el e . aspectRatio .= Commit n

-- | Make each axis have the same unit length.
equalAxis :: (Functor v, Num n) => AxisState b v n
equalAxis = axisScaling . mapped . aspectRatio .= Commit 1

-- axisPlotProperties :: IndexedTraversal' Int (Axis b v n) (PlotProperties b v n)
-- axisPlotProperties = axisPlots . traversed . _2ip
--   where
--     _2ip :: IndexPreservingLens' (Plot b v n, PlotProperties b v n)
--                                  (PlotProperties b v n)
--     _2ip = iplens (\(_,b)   -> b)
--                   (\(a,_) b -> (a,b))
    -- ipp :: IndexPreservingLens' (Plot b v n) (PlotProperties b v n)
    -- ipp = iplens (\(Plot _)   -> pp)
    --              (\(Plot a) pp -> Plot a pp)


-- Grid lines

-- | Set no major or minor grid lines for all axes.
noGridLines :: Functor v => AxisState b v n
noGridLines = noMajorGridLines >> noMinorGridLines

-- Majors

-- | Add major grid lines for all axes.
addMajorGridLines :: Functor v => AxisState b v n
addMajorGridLines = axisGridLines . mapped . majorGridF .= tickGridF

-- | Add major grid lines for given axis.
addMajorGridLine :: E v -> AxisState b v n
addMajorGridLine (E e) = axisGridLines . e . majorGridF .= tickGridF

-- | Set no major grid lines for all axes.
noMajorGridLines :: Functor v => AxisState b v n
noMajorGridLines = axisGridLines . mapped . majorGridF .= noGridF

-- | Set no major grid lines for given axis.
noMajorGridLine :: E v -> AxisState b v n
noMajorGridLine (E l) = axisGridLines . l . majorGridF .= noGridF

-- Minors

-- | Add minor grid lines for all axes.
addMinorGridLines :: Functor v => AxisState b v n
addMinorGridLines = axisGridLines . mapped . minorGridF .= tickGridF

-- | Add minor grid lines for given axis.
addMinorGridLine :: E v -> AxisState b v n
addMinorGridLine (E l) = axisGridLines . l . minorGridF .= tickGridF

-- | Set no minor grid lines for all axes.
noMinorGridLines :: Functor v => AxisState b v n
noMinorGridLines = axisGridLines . mapped . minorGridF .= noGridF

-- | Set no minor grid lines for given axis.
noMinorGridLine :: E v -> AxisState b v n
noMinorGridLine (E l) = axisGridLines . l . minorGridF .= noGridF

boundMin :: HasBounds a => E (V a) -> Lens' a (Recommend (N a))
boundMin (E l) = bounds . _Wrapped . l . lowerBound

boundMax :: HasBounds a => E (V a) -> Lens' a (Recommend (N a))
boundMax (E l) = bounds . _Wrapped . l . upperBound

xMin :: (HasBounds a, R1 (V a)) => Lens' a (Recommend (N a))
xMin = boundMin ex

xMax :: (HasBounds a, R1 (V a)) => Lens' a (Recommend (N a))
xMax = boundMax ex

yMin :: (HasBounds a, R2 (V a)) => Lens' a (Recommend (N a))
yMin = boundMin ey

yMax :: (HasBounds a, R2 (V a)) => Lens' a (Recommend (N a))
yMax = boundMin ey

zMin :: (HasBounds a, R3 (V a)) => Lens' a (Recommend (N a))
zMin = boundMin ey

zMax :: (HasBounds a, R3 (V a)) => Lens' a (Recommend (N a))
zMax = boundMin ey


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
