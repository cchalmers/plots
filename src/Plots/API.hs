{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE AllowAmbiguousTypes          #-}


{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}


module Plots.API
  ( -- * Data types and classes
    PointLike
  , Axis
  , r2Axis
  , renderAxis
  , Plot
  , PlotState
  , PlotStateM
  , R2Backend

    -- * Plotable
  , addPlotable
  , addPlotable'
  , addPlotableL
  , addPlotableL'

  , diagramPlot

    -- ** Bounds
  , xMin, xMax
  , yMin, yMax
  , zMin, zMax

    -- ** Grid lines
  , noGridLines
  , addLegendEntry

    -- ** Ticks

  -- , ticks

    -- Grid lines

  -- , recentProps
  , cartesianLabels

    -- ** Axis labels
    -- ** Axis title
    -- * Legend
  , addLegendEntry

    --  Themes
  -- , corperateTheme
  -- , blackAndWhiteTheme

    -- * Diagrams essentials
  , (#)
  , Diagram, V2 (..), V3 (..)
  , SizeSpec
  , lc
  , fc
  , fcA

    -- * Optics
    -- ** Basis elements
    -- | These basis elements can be used to select a specific coordinate axis.
    --   These can be used henever a function has a @E v@ argument.
  , ex, ey, ez

    -- ** Common functions
    -- | These lens functions can be used to change some of the more advanced
    --   aspects of the axis.
  , (&)
  , set, (.~)
  , over, (%~)
  , (&~)
  , (.=), assign
  , (%=), modify

    -- * Axis adjustments
    -- ** Axis size

   -- ** Axis labels
   -- *** Label text
  , axisLabel
  , xAxisLabel
  , yAxisLabel
  , zAxisLabel
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
  , noGridLine
  , setMajorGridLines
  , setMajorGridLine
  , noMajorGridLines
  , noMajorGridLine
  , setMinorGridLines
  , setMinorGridLine
  , noMinorGridLines
  , noMinorGridLine
  , allGridLines

    -- ** Axis Lines
  , middleAxisLines
  , boxAxisLines

    -- ** Axis ticks
  , noAxisTicks
  , noMajorTicks
  , noMinorTicks
  , centerAxisTicks
  , insideAxisTicks

    -- ** Axis theme
  , PlotStyle
  , plotColor
  , plotMarker
  , axisTheme
  , module Plots.Themes

    -- *** Colour bar
  , ColourBarOpts
  , ColourMap
  -- , C
  , showColourBar
  ) where

import           Control.Lens                    hiding (( # ))
import           Control.Monad.State.Lazy

import           Data.Default
import           Data.Monoid.Recommend
import           Data.Typeable

import           Diagrams.Coordinates.Isomorphic
import           Diagrams.Prelude                hiding (r2)
import           Diagrams.TwoD.Text

import           Linear

import           Plots.Axis
import           Plots.Axis.Grid
import           Plots.Axis.Labels
import           Plots.Axis.Render
import           Plots.Axis.Ticks
import           Plots.Axis.ColourBar

import           Plots.Types
import           Plots.Themes

-- import Plots.Types.Bar

-- import Plots.Types.Surface

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
-- This means whenever you see @PointLike (BaseSpace v) n p@ in a type constaint,
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


-- | Convienient type synonym for renderable types that all the standard
--   2D backends support.
type R2Backend b n =
  (Renderable (Path V2 n) b,
   Renderable (Text n) b,
   Typeable b,
   TypeableFloat n,
   Enum n)

-- type AxisStateM b v n = State (Axis b v n)
-- type AxisState b v n  = AxisStateM b v n ()

type PlotStateM a b = State (PropertiedPlot a b)

-- | The plot state allows you to use lenses for the plot @a@ as well as
--   the @PlotProperties@.
type PlotState a b  = PlotStateM a b ()

-- type PropertyStateM b v n a = State (PlotProperties b v n) a
-- type PropertyState b v n = State (PlotProperties b v n) ()

-- newtype PlotStateM p b v n = PState (State (p, PlotProperties))
--   deriving (Functor, Applicative, Monad, MonadState (P.Axis b v n))

------------------------------------------------------------------------
-- Plot properties
------------------------------------------------------------------------

-- $properties
-- Every plot has a assosiating 'PlotProperties'. These contain general
-- attributes like the legend entries and the style and the name for
-- the plot.
--
-- There are several ways to adjust the properties.

------------------------------------------------------------------------
-- Plotable
------------------------------------------------------------------------

-- $plotable
-- The 'Plotable' class defines ways of converting the data type to a
-- diagram for some axis. There are several variants for adding an axis
-- with constraints @('InSpace' v n a, 'Plotable' a b)@:
--
-- @
-- 'addPlotable'   ::           a ->                  'AxisState' b v n
-- 'addPlotable''  ::           a -> 'PlotState' a b -> 'AxisState' b v n
-- 'addPlotableL'  :: 'String' -> a ->                  'AxisState' b v n
-- 'addPlotableL'' :: 'String' -> a -> 'PlotState' a b -> 'AxisState' b v n
-- @
--
-- The last argument is a 'PlotState' so you can use @do@ notation to
-- make adjustments to the plot. The @L@ suffix stands for \"legend\",
-- it is equivalent of using 'addLegendEntry' in the 'PlotState'. Since
-- legend entries are so common it has it's own suffix. The following
-- are equivalent:
--
-- @
-- myaxis = 'r2Axis' &~ do
--   'addPlotable'' myplot $ do
--     'addLegendEntry' "my plot"
-- @
--
-- @
-- myaxis = 'r2Axis' &~ do
--   'addPlotableL' "my plot" myplot
-- @
--
-- Most of the time you won't use these functions directly. However,
-- other plotting functions follow this naming convention where instead
-- of @a@, it takes the data needed to make the plot.

-- | Add something 'Plotable' to the axis.
-- addPlotable :: (InSpace (BaseSpace v) n a, MoPlotable a b) => a -> AxisState b v n
addPlotable :: (InSpace (BaseSpace v) n a, MonadState (Axis b v n) m, Plotable a b)
            => a -> m ()
addPlotable a = axisPlots %= flip snoc (Plot' a mempty)

-- | Add something 'Plotable' and modify the 'PlotState' of that plot.
addPlotable' :: (InSpace (BaseSpace v) n a, MonadState (Axis b v n) m, Plotable a b)
             => a -> PlotState a b -> m ()
addPlotable' a s = axisPlots <>= [Plot' a (Endo $ execState s)]

-- | Add something 'Plotable' with given legend entry.
addPlotableL :: (InSpace (BaseSpace v) n a, MonadState (Axis b v n) m, Plotable a b)
             => String -> a -> m ()
addPlotableL l a = addPlotable' a $ addLegendEntry l

-- | Add something 'Plotable' with given legend entry and modify the
--   'PlotState' of that plot.
addPlotableL' :: (InSpace (BaseSpace v) n a, MonadState (Axis b v n) m, Plotable a b)
              => String -> a -> PlotState a b -> m ()
addPlotableL' l a s = addPlotable' a $ addLegendEntry l >> s

------------------------------------------------------------------------
-- Diagram Plot
------------------------------------------------------------------------

diagramPlot
  :: (v ~ BaseSpace c,
      Renderable (Path V2 n) b,
      MonadState (Axis b c n) m,
      Typeable b,
      Typeable v,
      Metric v,
      TypeableFloat n)
  => QDiagram b v n Any -> m ()
diagramPlot = addPlotable

------------------------------------------------------------------------
-- Axis properties
------------------------------------------------------------------------

-- | Traversal over the axis' most recent 'PlotProperties'.
-- recentProps :: PropertyState b v n -> AxisState b v n
-- recentProps s = axisPlots . _last . _2 %= (execState s .)

-- Legend
------------

addLegendEntry :: (MonadState a m, HasPlotProperties a b, Num (N a))
               => String -> m ()
addLegendEntry s = legendEntries <>= [mkLegendEntry s]

-- axisState :: Axis b v n -> AxisStateM b v n a -> Axis b v n
-- axisState a s = execState s a


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
--
--   @@
--   myaxis = 'r2Axis' &~ 'axisLabel' 'ex' "x-axis"
--   @@
axisLabel :: E v -> Lens' (Axis b v n) String
axisLabel (E e) = axisLabels . e . axisLabelText

-- | Lens onto the x axis label.
xAxisLabel :: R1 v => Lens' (Axis b v n) String
xAxisLabel = axisLabel ex

-- | Lens onto the y axis label.
yAxisLabel :: R2 v => Lens' (Axis b v n) String
yAxisLabel = axisLabel ey

-- | Lens onto the z axis label.
zAxisLabel :: R3 v => Lens' (Axis b v n) String
zAxisLabel = axisLabel ex

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
setAxesLabelGaps = axisLabels . traverse . axisLabelGap

-- | Label the x,y and z axis with \"x\", \"y\" and \"z\" respectively.
cartesianLabels :: (Traversable v, MonadState (Axis b v n) m) => m ()
cartesianLabels =
  partsOf (axisLabels . traverse . axisLabelText) .= ["x", "y", "z"]

-- | Set the aspect ratio of given axis.
setAxisRatio :: MonadState (Axis b v n) m => E v -> n -> m ()
setAxisRatio e n = axisScaling . el e . aspectRatio .= Commit n

-- | Make each axis have the same unit length.
equalAxis :: (MonadState (Axis b v n) m, Functor v, Num n) => m ()
equalAxis = axisScaling . mapped . aspectRatio .= Commit 1

------------------------------------------------------------------------
-- Grid lines
------------------------------------------------------------------------

-- | Set no major or minor grid lines for all axes.
noGridLines :: (Functor v, MonadState (Axis b v n) m) => m ()
noGridLines = noMajorGridLines >> noMinorGridLines

-- | Set no major or minor grid lines for given axes.
noGridLine :: MonadState (Axis b v n) m => E v -> m ()
noGridLine e = noMajorGridLine e >> noMinorGridLine e

-- Majors

-- | Add major grid lines for all axes.
setMajorGridLines :: (Functor v, MonadState (Axis b v n) m) => m ()
setMajorGridLines = axisGridLines . mapped . majorGridF .= tickGridF

-- | Add major grid lines for given axis.
setMajorGridLine :: MonadState (Axis b v n) m => E v -> m ()
setMajorGridLine (E e) = axisGridLines . e . majorGridF .= tickGridF

-- | Set no major grid lines for all axes.
noMajorGridLines :: (Functor v, MonadState (Axis b v n) m) => m ()
noMajorGridLines = axisGridLines . mapped . majorGridF .= noGridF

-- | Set no major grid lines for given axis.
noMajorGridLine :: MonadState (Axis b v n) m => E v -> m ()
noMajorGridLine (E l) = axisGridLines . l . majorGridF .= noGridF

-- Minors

-- | Add minor grid lines for all axes.
setMinorGridLines :: (Functor v, MonadState (Axis b v n) m) => m ()
setMinorGridLines = axisGridLines . mapped . minorGridF .= tickGridF

-- | Add minor grid lines for given axis.
setMinorGridLine :: MonadState (Axis b v n) m => E v -> m ()
setMinorGridLine (E l) = axisGridLines . l . minorGridF .= tickGridF

-- | Set no minor grid lines for all axes.
noMinorGridLines :: (Functor v, MonadState (Axis b v n) m) => m ()
noMinorGridLines = axisGridLines . mapped . minorGridF .= noGridF

-- | Set no minor grid lines for given axis.
noMinorGridLine :: MonadState (Axis b v n) m => E v -> m ()
noMinorGridLine (E l) = axisGridLines . l . minorGridF .= noGridF

-- | Traversal over both major and minor grid lines for all axes.
allGridLines :: Traversable v => Traversal' (Axis b v n) (GridLines v n)
allGridLines = axisGridLines . traverse

------------------------------------------------------------------------
-- Bounds
------------------------------------------------------------------------

boundMin :: HasBounds a c => E c -> Lens' a (Recommend (N a))
boundMin (E l) = bounds . _Wrapped . l . lowerBound

boundMax :: HasBounds a c => E c -> Lens' a (Recommend (N a))
boundMax (E l) = bounds . _Wrapped . l . upperBound

xMin :: (HasBounds a c, R1 c) => Lens' a (Recommend (N a))
xMin = boundMin ex

xMax :: (HasBounds a c, R1 c) => Lens' a (Recommend (N a))
xMax = boundMax ex

yMin :: (HasBounds a c, R2 c) => Lens' a (Recommend (N a))
yMin = boundMin ey

yMax :: (HasBounds a c, R2 c) => Lens' a (Recommend (N a))
yMax = boundMax ey

zMin :: (HasBounds a c, R3 c) => Lens' a (Recommend (N a))
zMin = boundMin ey

zMax :: (HasBounds a c, R3 c) => Lens' a (Recommend (N a))
zMax = boundMin ey

------------------------------------------------------------------------
-- Grid lines
------------------------------------------------------------------------

-- | Set all axis grid lines to form a box.
boxAxisLines :: (Functor v, MonadState (Axis b v n) m) => m ()
boxAxisLines =
  axisLines . mapped . axisLineType .= BoxAxisLine

-- | Set all axis grid lines to pass though the origin. If the origin is
--   not in bounds the line will be on the edge closest to the origin.
middleAxisLines :: (Functor v, MonadState (Axis b v n) m) => m ()
middleAxisLines =
  axisLines . mapped . axisLineType .= MiddleAxisLine

-- -- | Traversal over all axis line types.
-- axisLineTypes :: HasAxisLines a v => Tranversal' a AxisLineType
-- axisLineTypes = axisLines . traversed . axisLine

-- -- | Lens onto x axis line type.
-- xAxisLineType :: (L.R1 v, HasAxisLines a v) => Lens' a AxisLineType
-- xAxisLineType = axisLine ex . axisLineType

-- -- | Lens onto y axis line type.
-- yAxisLineType :: (L.V2 n v, HasAxisLines a v) => Lens' a AxisLineType
-- yAxisLineType = axisLine ey . axisLineType

-- -- | Lens onto z axis line type.
-- zAxisLineType :: (L.V3 n v, HasAxisLines a v) => Lens' a AxisLineType
-- zAxisLineType = axisLine ez . axisLineType

-- xAxisArrowOpts :: (L.R1 v, HasAxisLines a v) => Lens' a (Maybe ArrowOpts)
-- xAxisArrowOpts = axisLine ex . axisArrowOpts

-- yAxisArrowOpts :: (L.V2 n v, HasAxisLines a v) => Lens' a (Maybe ArrowOpts)
-- yAxisArrowOpts = axisLine ey . axisArrowOpts

-- zAxisArrowOpts :: (L.V3 n v, HasAxisLines a v) => Lens' a (Maybe ArrowOpts)
-- zAxisArrowOpts = axisLines ez . axisArrowOpts
--

------------------------------------------------------------------------
-- Ticks
------------------------------------------------------------------------

-- | Remove minor ticks from all axes.
noMinorTicks :: (Functor v, MonadState (Axis b v n) m) => m ()
noMinorTicks =
  axisTicks . mapped . minorTickAlign .= noTicks

-- | Remove major ticks from all axes.
noMajorTicks :: (Functor v, MonadState (Axis b v n) m) => m ()
noMajorTicks =
  axisTicks . mapped . majorTickAlign .= noTicks

-- | Remove both major and minor ticks from all axes.
noAxisTicks :: (Functor v, MonadState (Axis b v n) m) => m ()
noAxisTicks = noMinorTicks >> noMajorTicks

centerAxisTicks :: (Functor v, MonadState (Axis b v n) m) => m ()
centerAxisTicks =
  axisTicks . mapped . tickAlign .= centerTicks

insideAxisTicks :: (Functor v, MonadState (Axis b v n) m) => m ()
insideAxisTicks =
  axisTicks . mapped . tickAlign .= insideTicks


------------------------------------------------------------------------
-- Style
------------------------------------------------------------------------

-- $style
-- Styles are a key part of a plot. It defines properties like colours
-- and markers for each plot. The default way a plot gets it's style is
-- from the axis theme. This is a list of plot styles that is zipped
-- with the plots when the axis is rendered.

------------------------------------------------------------------------
-- Colour bar
------------------------------------------------------------------------

-- $colourbar
-- The 'ColourBar' provides a visual key between a colour and value. The
-- colour bar is not shown by default, it either needs a plot like
-- 'addHeatMap' to turn it on or it can be turned on explicitly by
-- 'showColorBar'.

showColourBar :: MonadState (Axis b v n) m => m ()
showColourBar = axisColourBar . cbShow .= True


{-# ANN module ("HLint: ignore Use import/export shortcut" :: String) #-}
