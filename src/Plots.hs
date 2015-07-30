{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE AllowAmbiguousTypes          #-}


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
  ( -- * Data types and classes
    PointLike
  , Axis
  , r2Axis
  , renderAxis
  , Plot
  , PlotState

    -- * Plotable
    -- $plotable
  , addPlotable
  , addPlotable'
  , addPlotableL
  , addPlotableL'

--  , _LinePlot'
--  , _ScatterPlot'
    -- ** Scatter plot
  , ScatterPlot
  , scatterPlot
  , scatterPlot'
  , scatterPlotL
  , scatterPlotOf
  , scatterPlotOf'
  , scatterPlotLOf
  , module Plots.Types.Scatter

  , gscatterPlot
  , gscatterPlot'
  , gscatterPlotL
  -- **fold variant
  --, gscatterPlotOf
  --, gscatterPlotOf'
  --, gscatterPlotLOf

    -- ** Line plot
  , Path
  , linePlot''
  , pathPlot''

  , LinePlot
  , linePlot
  , linePlot'
  , linePlotL
  , linePlotOf
  , linePlotOf'
  , linePlotLOf
  , module Plots.Types.Line

  , ribbonPlot
  , ribbonPlot'
  , ribbonPlotL
  , ribbonPlotOf
  , ribbonPlotOf'
  , ribbonPlotLOf
  , module Plots.Types.Ribbon

  , barPlot
  , barPlot'
  , barPlotL

  , barPlotNormal
  , barPlotNormalC
  , barPlotNormalCL
  , barPlotNormalMulti
  , barPlotNormalMultiC

  , barPlotStacked
  , barPlotStackedC
  , barPlotStackedCL
  , barPlotStackedMultiC

  , barPlotSplit
  , barPlotSplitC
  , barPlotSplitCL
  , barPlotSplitMultiC

  , barPlotRatio
  , barPlotRatioC
  , barPlotRatioCL
  , barPlotRatioMultiC

  , histogramPlot
  , histogramPlot'
  , histogramPlotL
  , histogramPlotOf
  , histogramPlotOf'
  , histogramPlotLOf
  , module Plots.Types.Histogram

--  , barPlotNormal'
  --, module Plots.Types.Bar

  --, createstep
  , stepPlot
  , stepPlot'
  , stepPlotL

  , glinePlot
  , glinePlot'
  , glinePlotL

  , createlinerangev
  , linerangevPlot
  , linerangevPlot'
  , linerangevPlotL
  , linerangevPlotwithPoint

  , createlinerangeh
  , linerangehPlot
  , linerangehPlot'
  , linerangehPlotL
  , linerangehPlotwithPoint

  , errorbarvPlot
  , errorbarvPlotwithPoint

  , errorbarhPlot
  , errorbarhPlotwithPoint

  , crossbarvPlot
  , crossbarvPlotwithPoint

  , crossbarhPlot
  , crossbarhPlotwithPoint

  , boxplotvPlot
  , boxplothPlot

  , makeribbon
  , makearea
  , makearea'
  , makeareagroup'
  -- ** Line plot
  -- , addLinePlot
  -- , linePlot
  -- , multiLinePlot
  -- , linePlotFromPath

  -- , pathPlot'
  -- , pathPlotL
  -- , pathPlotL'
  , module Plots.Types.Line

    -- ** Diagram plot
  , diagramPlot

  -- ** Parametric plot
  --
  , parametricPlot
  , parametricRangePlot

  , parametricPlot'
  , parametricRangePlot'

  , parametricPlotL
  , parametricRangePlotL

  , abLinePlot
  , hLinePlot
  , vLinePlot

  , vectorPlot
  , vectorPointPlot
  , vectorPointPlot'
  , vectorPointPlot''
  , vectorPointPlotL
  , vectorFieldPlot
  -- ** Mesh plot
  -- , meshPlot
  -- , surfacePlot

    -- ** Function plot
    -- | Plot a given function.
  -- , addFunctionPlot
   , module Plots.Types.Function
   , module Plots.Types.Smooth
   , module Plots.Types.Density
   , module Plots.Types.Boxplot
   , smoothPlot
   , densityPlot'
   , boxPlot
    -- ** Bar plot
    -- | Bar plot
  -- , addBarPlot
  -- , barPlotAxis
  -- , module Plots.Types.Bar

    -- * Axis properties

    -- ** Bounds
  , xMin, xMax
  , yMin, yMax
  , zMin, zMax

    -- ** Grid lines
  , noGridLines
  , addLegendEntry

    -- ** Ticks

  -- , ticks

    -- ** Grid lines

  -- , recentProps
  , cartesianLabels

    -- ** Axis labels
    -- ** Axis title
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
import qualified Data.Foldable as F
import           Data.List
import           Data.Function

import           Diagrams.Coordinates.Isomorphic
import           Diagrams.Prelude
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

import           Plots.Types.Histogram
import           Plots.Types.Line
import           Plots.Types.Scatter
import           Plots.Types.Ribbon

import           Plots.Types.Function
import           Plots.Types.Smooth
import           Plots.Types.Density
import           Plots.Types.Boxplot

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
------------------------------------------------------------------------
--_LinePlot' :: Plotable (GLinePlot v n a) b => Traversal' (Plot' b v n) (GLinePlot v n a)
--_LinePlot' = _Plot'

--_ScatterPlot' :: Plotable (GScatterPlot v n a) b => Traversal' (Plot' b v n) (GScatterPlot v n a)
--_ScatterPlot' = _Plot'

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

------------------------------------------------------------------------
-- Density Plot
------------------------------------------------------------------------

densityPlot'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (DensityPlot v n) b,
      F.Foldable f ,
      Enum n, TypeableFloat n)
  => f p -> PlotState (DensityPlot v n) b -> m ()
densityPlot' d = addPlotable' (mkDensityPlot d)

------------------------------------------------------------------------
-- Smooth Plot
------------------------------------------------------------------------

smoothPlot
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (SmoothPlot v n) b,
      F.Foldable f ,
      Enum n, TypeableFloat n)
  => f p -> m ()
smoothPlot d = addPlotable (mkSmoothPlot d)

------------------------------------------------------------------------
-- Vector Plot
------------------------------------------------------------------------

vectorPlot
  :: (v ~ BaseSpace c,
      MonadState (Axis b c n) m,
      Plotable (VectorPlot v n) b,
      Additive v, TypeableFloat n)
  =>  v n -> m ()
vectorPlot f = addPlotable (mkVectorPlot f)

vectorPointPlot
  :: (v ~ BaseSpace c,
      MonadState (Axis b c n) m,
      Plotable (VectorPlot v n) b,
      Additive v, TypeableFloat n)
  =>  v n -> (n, n)  -> m ()
vectorPointPlot f d = addPlotable (mkVectorPointPlot f d)

vectorPointPlot'
  :: (v ~ BaseSpace c,
      MonadState (Axis b c n) m,
      Plotable (VectorPlot v n) b,
      Additive v, TypeableFloat n)
  =>  v n -> (n, n) -> PlotState (VectorPlot v n) b -> m ()
vectorPointPlot' f d = addPlotable' (mkVectorPointPlot f d)

vectorPointPlot''
  :: (v ~ BaseSpace c,
      MonadState (Axis b c n) m,
      Plotable (VectorPlot v n) b,
      Additive v, TypeableFloat n)
  =>  v n -> (n, n) -> ArrowOpts n -> m ()
vectorPointPlot'' f d opts = addPlotable' (mkVectorPointPlot f d) $ do
                              setArrowOpts .= opts

vectorPointPlotL
  :: (v ~ BaseSpace c,
      MonadState (Axis b c n) m,
      Plotable (VectorPlot v n) b,
      Additive v, TypeableFloat n)
  =>  String -> v n -> (n, n) -> m ()
vectorPointPlotL l f d = addPlotableL l (mkVectorPointPlot f d)

vectorFieldPlot
  :: (v ~ BaseSpace c,
      MonadState (Axis b c n) m,
      Plotable (VectorPlot v n) b,
      Additive v, TypeableFloat n)
  =>  [v n] -> [(n, n)] -> ArrowOpts n -> m ()
vectorFieldPlot vs ps opts = F.for_ (zip vs ps) $ \x -> vectorPointPlot'' (fst x) (snd x) opts

------------------------------------------------------------------------
-- Parametric Plot
------------------------------------------------------------------------

parametricPlot
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (ParametricPlot v n) b,
      Additive v, TypeableFloat n)
  => (n -> p) -> m ()
parametricPlot f = addPlotable (mkParametricPlot f)

parametricRangePlot
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (ParametricPlot v n) b,
      Additive v, TypeableFloat n)
  => (n -> p) -> (n ,n) -> m ()
parametricRangePlot f d = addPlotable (mkParametricRangePlot f d)

parametricPlot'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (ParametricPlot v n) b,
      Additive v, TypeableFloat n)
  => (n -> p) -> PlotState (ParametricPlot v n) b -> m ()
parametricPlot' f = addPlotable' (mkParametricPlot f)

parametricRangePlot'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (ParametricPlot v n) b,
      Additive v, TypeableFloat n)
  => (n -> p) -> (n ,n) -> PlotState (ParametricPlot v n) b -> m ()
parametricRangePlot' f d = addPlotable' (mkParametricRangePlot f d)

parametricPlotL
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (ParametricPlot v n) b,
      Additive v, TypeableFloat n)
  => String -> (n -> p) -> m ()
parametricPlotL l f = addPlotableL l (mkParametricPlot f)

parametricRangePlotL
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (ParametricPlot v n) b,
      Additive v, TypeableFloat n)
  => String -> (n -> p) -> (n ,n) -> m ()
parametricRangePlotL l f d = addPlotableL l (mkParametricRangePlot f d)

abLinePlot
  :: (v ~ BaseSpace c,
      PointLike v n (P2 n),
      MonadState (Axis b c n) m,
      Plotable (ParametricPlot v n) b,
      Additive v, TypeableFloat n)
     => n -> n -> (n ,n) -> m ()
abLinePlot slope intercept d = addPlotable (mkParametricRangePlot (createABLine slope intercept) d)

hLinePlot
  :: (v ~ BaseSpace c,
      PointLike v n (P2 n),
      MonadState (Axis b c n) m,
      Plotable (ParametricPlot v n) b,
      Additive v, TypeableFloat n)
     => n -> (n ,n) -> m ()
hLinePlot intercept d = addPlotable (mkParametricRangePlot (createHLine intercept) d)

vLinePlot
  :: (v ~ BaseSpace c,
      PointLike v n (P2 n),
      MonadState (Axis b c n) m,
      Plotable (ParametricPlot v n) b,
      Additive v, TypeableFloat n)
     => n -> (n ,n) -> m ()
vLinePlot intercept d = addPlotable (mkParametricRangePlot (createVLine intercept) d)
 
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
-- @
--   myaxis = r2Axis ~&
--     scatterPlot data1
-- @
scatterPlot
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (ScatterPlot v n) b,
      F.Foldable f)
  => f p -> m ()
scatterPlot d = addPlotable (mkScatterPlot d)

-- | Make a 'ScatterPlot' and take a 'State' on the plot to alter it's
--   options
--
-- @
--   myaxis = r2Axis &~ do
--     scatterPlot' pointData1 $ do
--       connectingLine .= True
--       addLegendEntry "data 1"
-- @
scatterPlot'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (ScatterPlot v n) b,
      F.Foldable f)
  => f p -> PlotState (ScatterPlot v n) b -> m ()
scatterPlot' d = addPlotable' (mkScatterPlot d)

-- | Add a 'ScatterPlot' with the given name for the legend entry.
--
-- @
--   myaxis = r2Axis &~ do
--     scatterPlotL "blue team" pointData1
--     scatterPlotL "red team" pointData2
-- @
scatterPlotL
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (ScatterPlot v n) b,
      F.Foldable f)
  => String -> f p -> m ()
scatterPlotL l d = addPlotableL l (mkScatterPlot d)

-- Fold variants

scatterPlotOf
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (ScatterPlot v n) b)
  => Fold s p -> s -> m ()
scatterPlotOf f s = addPlotable (mkScatterPlotOf f s)

scatterPlotOf'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (ScatterPlot v n) b)
  => Fold s p -> s -> PlotState (ScatterPlot v n) b -> m ()
scatterPlotOf' f s = addPlotable' (mkScatterPlotOf f s)

scatterPlotLOf
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (ScatterPlot v n) b)
  => String -> Fold s p -> s -> m ()
scatterPlotLOf l f s = addPlotableL l (mkScatterPlotOf f s)

------------------------------------------------------------------------
-- Bubble plot -- ??
------------------------------------------------------------------------

-- $ bubble
-- Scatter plots with extra numeric parameter. By default the extra
-- parameter is the scale of the marker but this can be changed.

-- bubblePlot :: (PointLike (BaseSpace v) n p, R2Backend b n, Plotable (P.ScatterPlot v n) b, F.Foldable f)
--             => f (n,p) -> AxisState b v n
-- bubblePlot d = axisPlots <>= [P.Plot (P.mkBubblePlot d) def]

-- bubblePlot' :: (PointLike (BaseSpace v) n p, R2Backend b n, Plotable (P.ScatterPlot v n) b, F.Foldable f)
--             => f (n,p) -> AxisState b v n
-- bubblePlot' d s = axisPlots <>= [P.Plot (execState s $ P.mkBubblePlot d) def]

------------------------------------------------------------------------
-- GScatterPlot
------------------------------------------------------------------------

gscatterPlot
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (GScatterPlot v n a) b,
      F.Foldable f)
  => f a -> (a -> p) -> m ()
gscatterPlot d pf = addPlotable (mkGScatterPlot d pf)

gscatterPlot'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (GScatterPlot v n a) b,
      F.Foldable f)
  => f a -> (a -> p) -> PlotState (GScatterPlot v n a) b -> m ()
gscatterPlot' d pf = addPlotable' (mkGScatterPlot d pf)


gscatterPlotL
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (GScatterPlot v n a) b,
      F.Foldable f)
  => String -> f a -> (a -> p) -> m ()
gscatterPlotL l d pf = addPlotableL l (mkGScatterPlot d pf)

------------------------------------------------------------------------
-- Line plot
------------------------------------------------------------------------

-- $ line
-- line plots display data as dots. There are several representations
-- for line plots for extra parameters. Line plots have the
-- following lenses:
--
-- @
-- * 'dotsonPoint' :: 'Lens'' ('LinePlot' v n) 'Bool' - False
-- * 'lineStyle': 'Maybe' ('Point' v n -> 'Style' 'V2' n) - Nothing
-- @
--

-- | Add a 'LinePlot' to the 'AxisState' from a data set.
--
-- @
--   myaxis = r2Axis ~&
--     linePlot data1
-- @

linePlot''
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      R2Backend b n,
      Plotable (Path v n) b,
      F.Foldable f)
  => f p -> m ()
linePlot'' d = addPlotable (mkPath $ Identity d)

pathPlot'' :: (R2Backend b n, MonadState (Axis b V2 n) m) => Path V2 n -> m ()
pathPlot'' = addPlotable

--------------------------------------------------------------------------

linePlot
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (LinePlot v n) b,
      F.Foldable f)
  => f p -> m ()
linePlot d = addPlotable (mkLinePlot d)

-- | Make a 'LinePlot' and take a 'State' on the plot to alter it's
--   options
--
linePlot'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (LinePlot v n) b,
      F.Foldable f)
  => f p -> PlotState (LinePlot v n) b -> m ()
linePlot' d = addPlotable' (mkLinePlot d)

-- | Add a 'LinePlot' with the given name for the legend entry.
--
-- @
--   myaxis = r2Axis &~ do
--     linePlotL "blue team" pointData1
--     linePlotL "red team" pointData2
-- @
linePlotL
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (LinePlot v n) b,
      F.Foldable f)
  => String -> f p -> m ()
linePlotL l d = addPlotableL l (mkLinePlot d)

-- Fold variants

linePlotOf
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (LinePlot v n) b)
  => Fold s p -> s -> m ()
linePlotOf f s = addPlotable (mkLinePlotOf f s)

linePlotOf'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (LinePlot v n) b)
  => Fold s p -> s -> PlotState (LinePlot v n) b -> m ()
linePlotOf' f s = addPlotable' (mkLinePlotOf f s)

linePlotLOf
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (LinePlot v n) b)
  => String -> Fold s p -> s -> m ()
linePlotLOf l f s = addPlotableL l (mkLinePlotOf f s)

------------------------------------------------------------------------
--Step
------------------------------------------------------------------------

stepPlot :: (RealFloat n, Typeable n, Typeable b, Renderable (Path V2 n) b,
             MonadState (Axis b c n) m, BaseSpace c ~ V2)
         => [(n, n)] -> m ()
stepPlot  a   = linePlot (createStepData a)

stepPlot' :: (RealFloat n, Typeable n, Typeable b, Renderable (Path V2 n) b,
              MonadState (Axis b c n) m, BaseSpace c ~ V2) =>
             [(n, n)] -> PlotState (LinePlot V2 n) b -> m ()
stepPlot' a   = linePlot' (createStepData a)

stepPlotL :: (RealFloat n, Typeable n, Typeable b, Renderable (Path V2 n) b,
              MonadState (Axis b c n) m, BaseSpace c ~ V2) =>
             String -> [(n, n)] -> m ()
stepPlotL l a = linePlotL l (createStepData a)

------------------------------------------------------------------------
-- General Line Plot
------------------------------------------------------------------------


glinePlot
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (GLinePlot v n a) b,
      F.Foldable f)
  => f a -> (a -> p) -> m ()
glinePlot d pf = addPlotable (mkGLinePlot d pf)

glinePlot'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (GLinePlot v n a) b,
      F.Foldable f)
  => f a -> (a -> p) -> PlotState (GLinePlot v n a) b -> m ()
glinePlot' d pf = addPlotable' (mkGLinePlot d pf)

glinePlotL
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (GLinePlot v n a) b,
      F.Foldable f)
  => String -> f a -> (a -> p) -> m ()
glinePlotL l d pf = addPlotableL l (mkGLinePlot d pf)

------------------------------------------------------------------------
-- Linerange Vertical
------------------------------------------------------------------------
createlinerangev :: (Double, Double) -> Double -> [(Double, Double)]
createlinerangev (a, b) s = [(a+(s/2), b), (a-(s/2), b)]

linerangevPlot :: (Typeable b, Renderable (Path V2 Double) b,
                   MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                  (Double, Double) -> Double -> m ()
linerangevPlot a s  = linePlot (createlinerangev a s)

linerangevPlot' :: (Typeable b, Renderable (Path V2 Double) b,
                    MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                   (Double, Double) -> Double -> m ()
linerangevPlot' a s  = linePlot (createlinerangev a s)

linerangevPlotL :: (Typeable b, Renderable (Path V2 Double) b,
                    MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                   String -> (Double, Double) -> Double -> m ()
linerangevPlotL l a s = linePlotL l (createlinerangev a s)

linerangevPlotwithPoint :: (Typeable b, Renderable (Path V2 Double) b,
                            MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                           (Double, Double) -> Double -> m ()
linerangevPlotwithPoint a s = do
     linePlot' (createlinerangev a s) $ do
        plotColor .= purple
        addLegendEntry "data 1"
     scatterPlot' [a] $ do
        plotColor .= purple
        plotMarker %= scale 2

-- add more api with option for colour
-- add group for line step, linerange

------------------------------------------------------------------------
-- Linerange Horizontal
------------------------------------------------------------------------
createlinerangeh :: (Double, Double) -> Double -> [(Double, Double)]
createlinerangeh (a, b) s = [(a, b+(s/2)), (a, b-(s/2))]

linerangehPlot :: (Typeable b, Renderable (Path V2 Double) b,
                   MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                  (Double, Double) -> Double -> m ()
linerangehPlot a s  = linePlot (createlinerangeh a s)

linerangehPlot' :: (Typeable b, Renderable (Path V2 Double) b,
                    MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                   (Double, Double) -> Double -> m ()
linerangehPlot' a s  = linePlot (createlinerangeh a s)

linerangehPlotL :: (Typeable b, Renderable (Path V2 Double) b,
                    MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                   String -> (Double, Double) -> Double -> m ()
linerangehPlotL l a s = linePlotL l (createlinerangeh a s)

linerangehPlotwithPoint :: (Typeable b, Renderable (Path V2 Double) b,
                            MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                           (Double, Double) -> Double -> m ()
linerangehPlotwithPoint a s = do
     linePlot' (createlinerangeh a s) $ do
        plotColor .= purple
        addLegendEntry "data 1"
     scatterPlot' [a] $ do
        plotColor .= purple
        plotMarker %= scale 2

-- add more api with option for colour

------------------------------------------------------------------------
-- Errorbar Vertical
------------------------------------------------------------------------
-- figure out way to use same plotcolour

errorbarvPlot :: (Typeable b, Renderable (Path V2 Double) b,
                  MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                 (Double, Double) -> Double -> Double -> m ()
errorbarvPlot (a,b) s h = do
              linePlot' (createlinerangev (a, b) s) $ do
                 plotColor .= red
                 addLegendEntry "data 1"
              linePlot' (createlinerangeh (a+(s/2), b) h) $ do
                 plotColor .= red
              linePlot' (createlinerangeh (a-(s/2), b) h) $ do
                 plotColor .= red

errorbarvPlotwithPoint :: (Typeable b, Renderable (Path V2 Double) b,
                           MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                          (Double, Double) -> Double -> Double -> m ()
errorbarvPlotwithPoint (a,b) s h = do
              linePlot' (createlinerangev (a, b) s) $ do
                 plotColor .= blue
                 addLegendEntry "data 1"
              linePlot' (createlinerangeh (a+(s/2), b) h) $ do
                 plotColor .= blue
              linePlot' (createlinerangeh (a-(s/2), b) h) $ do
                 plotColor .= blue
              scatterPlot' [(a,b)] $ do
                 plotColor .= blue



------------------------------------------------------------------------
-- Errorbar Horizontal
------------------------------------------------------------------------

errorbarhPlot :: (Typeable b, Renderable (Path V2 Double) b,
                  MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                 (Double, Double) -> Double -> Double -> m ()
errorbarhPlot (a,b) s h = do
              linePlot' (createlinerangeh (a, b) s) $ do
                 plotColor .= red
                 addLegendEntry "data n"
              linePlot' (createlinerangev (a, b+(s/2)) h) $ do
                 plotColor .= red
              linePlot' (createlinerangev (a, b-(s/2)) h) $ do
                 plotColor .= red

errorbarhPlotwithPoint :: (Typeable b, Renderable (Path V2 Double) b,
                           MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                          (Double, Double) -> Double -> Double -> m ()
errorbarhPlotwithPoint (a,b) s h = do
              linePlot' (createlinerangeh (a, b) s) $ do
                 plotColor .= blue
                 addLegendEntry "data n"
              linePlot' (createlinerangev (a, b+(s/2)) h) $ do
                 plotColor .= blue
              linePlot' (createlinerangev (a, b-(s/2)) h) $ do
                 plotColor .= blue
              scatterPlot' [(a,b)] $ do
                 plotColor .= blue

------------------------------------------------------------------------
-- Crossbar Vertical
------------------------------------------------------------------------

crossbarvPlot :: (Typeable b, Renderable (Path V2 Double) b,
                  MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                 (Double, Double) -> Double -> Double -> m ()
crossbarvPlot (a,b) s h = do
              linePlot' (createlinerangeh (a, b) h) $ do
                 plotColor .= red
                 addLegendEntry "data n"
              linePlot' (createlinerangev (a, b+(h/2)) s) $ do
                 plotColor .= red
              linePlot' (createlinerangev (a, b-(h/2)) s) $ do
                 plotColor .= red
              linePlot' (createlinerangeh (a+(s/2), b) h) $ do
                 plotColor .= red
              linePlot' (createlinerangeh (a-(s/2), b) h) $ do
                 plotColor .= red

crossbarvPlotwithPoint :: (Typeable b, Renderable (Path V2 Double) b,
                           MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                          (Double, Double) -> Double -> Double -> m ()
crossbarvPlotwithPoint (a,b) s h = do
              linePlot' (createlinerangeh (a, b) h) $ do
                 plotColor .= blue
                 addLegendEntry "data n"
              linePlot' (createlinerangev (a, b+(h/2)) s) $ do
                 plotColor .= blue
              linePlot' (createlinerangev (a, b-(h/2)) s) $ do
                 plotColor .= blue
              linePlot' (createlinerangeh (a+(s/2), b) h) $ do
                 plotColor .= blue
              linePlot' (createlinerangeh (a-(s/2), b) h) $ do
                 plotColor .= blue
              scatterPlot' [(a,b)] $ do
                 plotColor .= blue

------------------------------------------------------------------------
-- Crossbar Horizontal  --options for colour and legends seperate
------------------------------------------------------------------------

crossbarhPlot :: (Typeable b, Renderable (Path V2 Double) b,
                  MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                 (Double, Double) -> Double -> Double -> m ()
crossbarhPlot (a,b) s h = do
              linePlot' (createlinerangev (a, b) s) $ do
                 plotColor .= red
                 addLegendEntry "data n"
              linePlot' (createlinerangev (a, b+(h/2)) s) $ do
                 plotColor .= red
              linePlot' (createlinerangev (a, b-(h/2)) s) $ do
                 plotColor .= red
              linePlot' (createlinerangeh (a+(s/2), b) h) $ do
                 plotColor .= red
              linePlot' (createlinerangeh (a-(s/2), b) h) $ do
                 plotColor .= red

crossbarhPlotwithPoint :: (Typeable b, Renderable (Path V2 Double) b,
                           MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                          (Double, Double) -> Double -> Double -> m ()
crossbarhPlotwithPoint (a,b) s h = do
              linePlot' (createlinerangev (a, b) s) $ do
                 plotColor .= blue
                 addLegendEntry "data n"
              linePlot' (createlinerangev (a, b+(h/2)) s) $ do
                 plotColor .= blue
              linePlot' (createlinerangev (a, b-(h/2)) s) $ do
                 plotColor .= blue
              linePlot' (createlinerangeh (a+(s/2), b) h) $ do
                 plotColor .= blue
              linePlot' (createlinerangeh (a-(s/2), b) h) $ do
                 plotColor .= blue
              scatterPlot' [(a,b)] $ do
                 plotColor .= blue
------------------------------------------------------------------------
-- Ribbon
------------------------------------------------------------------------
ribbonPlot
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (RibbonPlot v n) b,
      F.Foldable f)
  => f p -> m ()
ribbonPlot a = addPlotable (mkRibbonPlot a)

ribbonPlot'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (RibbonPlot v n) b,
      F.Foldable f)
  => f p -> PlotState (RibbonPlot v n) b -> m ()
ribbonPlot' d = addPlotable' (mkRibbonPlot d)

ribbonPlotL
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (RibbonPlot v n) b,
      F.Foldable f)
  => String -> f p -> m ()
ribbonPlotL l d = addPlotableL l (mkRibbonPlot d)

-- Fold variants

ribbonPlotOf
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (RibbonPlot v n) b)
  => Fold s p -> s -> m ()
ribbonPlotOf f s = addPlotable (mkRibbonPlotOf f s)

ribbonPlotOf'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (RibbonPlot v n) b)
  => Fold s p -> s -> PlotState (RibbonPlot v n) b -> m ()
ribbonPlotOf' f s = addPlotable' (mkRibbonPlotOf f s)

ribbonPlotLOf
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (RibbonPlot v n) b)
  => String -> Fold s p -> s -> m ()
ribbonPlotLOf l f s = addPlotableL l (mkRibbonPlotOf f s)

------------------------------------------------------------------------
-- Area
------------------------------------------------------------------------

makeribbon :: (RealFloat b, Typeable b, Typeable b1, Renderable (Path V2 b) b1,
               MonadState (Axis b1 c b) m, BaseSpace c ~ V2) =>
              [b] -> [b] -> [b] -> Colour Double -> m ()
makeribbon x1s x2s ys colour = ribbonPlot' ((zip x1s ys) ++ reverse (zip x2s ys)) $ do
                                 addLegendEntry "ribbon test"
                                 plotColor .= colour

makearea :: (Typeable b, Renderable (Path V2 Double) b,
             MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
            [Double] -> [Double] -> Colour Double -> m ()
makearea xs ys colour = ribbonPlot' ((zip xs ys) ++ reverse (zeroY (zip xs ys))) $ do
                                 addLegendEntry "ribbon test"
                                 plotColor .= colour

makearea' :: (Typeable b, Renderable (Path V2 Double) b,
              MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
             [(Double, Double)] -> String -> m ()
makearea' sd string = ribbonPlot' (sd ++ reverse (zeroY sd)) $ do
                                 addLegendEntry string

-- makeareagroup xs ys ds = makeareagroup' (creategroupdata (zip xs ys) ds)
-- error when using group

makeareagroup' :: (Typeable b, Renderable (Path V2 Double) b,
                   MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                  m ([(Double, Double)], String) -> m ()
makeareagroup' xs  = do x <- xs
                        makearea' (fst x) (snd x)

------------------------------------------------------------------------
-- Bar
------------------------------------------------------------------------

barPlot :: (Typeable b, Renderable (Path V2 Double) b,
             MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
            (Double,Double) -> Double -> m ()
barPlot x w = ribbonPlot (createBarData x w)

barPlot' :: (Typeable b, Renderable (Path V2 Double) b,
             MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
            (Double,Double) -> Double -> PlotState (RibbonPlot V2 Double) b -> m ()
barPlot' x w = ribbonPlot' (createBarData x w)

barPlotC :: (Typeable b, Renderable (Path V2 Double) b,
             MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
            (Double,Double) -> Double -> Colour Double -> m ()
barPlotC x w colour = ribbonPlot' (createBarData x w) $ do
                                plotColor .= colour

barPlotL :: (Typeable b, Renderable (Path V2 Double) b,
             MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
            (Double,Double) -> Double -> String -> Colour Double -> m ()
barPlotL x w string colour = ribbonPlot' (createBarData x w) $ do
                                plotColor .= colour
                                addLegendEntry string

------------------------------------------------------------------------
-- Normal Bar --figure out a way to loop and use sort
------------------------------------------------------------------------
barPlotNormal :: (Typeable b, Renderable (Path V2 Double) b,
                  MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                 Int -> [Double] -> Double -> m ()
barPlotNormal y xs w = F.for_ xs $ \x -> barPlot ((fromIntegral y), x) w


barPlotNormalC :: (Typeable b, Renderable (Path V2 Double) b,
                  MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                 Int -> [Double] -> [Colour Double]-> Double -> m ()
barPlotNormalC y xs cs w = F.for_ (sortBy (compare `on` fst)(zip xs cs)) $ \x -> barPlotC ((fromIntegral y), fst x) w (snd x) 


barPlotNormalCL :: (Typeable b, Renderable (Path V2 Double) b,
                  MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                 Int -> [Double] -> [Colour Double] -> [String]-> Double -> m ()
barPlotNormalCL y xs cs ls w = F.for_ (sortBy (compare `on` fstof3)(zip3 xs cs ls)) $ \x -> barPlotL ((fromIntegral y), fstof3 x) w (trdof3 x) (sndof3 x) 

fstof3 (a, _, _) = a
sndof3 (_, a, _) = a
trdof3 (_, _, a) = a

barPlotNormalMulti :: (Typeable b, Renderable (Path V2 Double) b,
                  MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                  [[Double]] -> Double -> m ()
barPlotNormalMulti xs w = F.for_ [1 .. length xs] $ \x -> barPlotNormal x (xs!!(x-1)) w 

barPlotNormalMultiC :: (Typeable b, Renderable (Path V2 Double) b,
                  MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                  [[Double]] -> [Colour Double] -> [String] -> Double -> m ()
barPlotNormalMultiC xs colormap names w = do 
                                          barPlotNormalCL 1 (xs!!0) colormap names w
                                          F.for_ [2 .. length xs] $ \x -> barPlotNormalC x (xs!!(x-1)) colormap w 

------------------------------------------------------------------------
-- Stacked Bar
------------------------------------------------------------------------
barPlotStacked :: (Typeable b, Renderable (Path V2 Double) b,
                  MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                 Int -> [Double] -> Double -> m ()
barPlotStacked y xs w = F.for_ (additive xs) $ \x -> barPlot ((fromIntegral y), x) w

additive :: [Double] -> [Double]
additive [] = []
additive (x:[]) = [x]
additive (x:xs) = x:(additive (addtofst x xs))

addtofst :: Double -> [Double] -> [Double]
addtofst a (x:xs) = (a+x):xs

barPlotStackedC :: (Typeable b, Renderable (Path V2 Double) b,
                  MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                 Int -> [Double] -> [Colour Double]-> Double -> m ()
barPlotStackedC y xs cs w = F.for_ (sortBy (compare `on` fst)(zip (additive xs) cs)) $ \x -> barPlotC ((fromIntegral y), fst x) w (snd x) 

barPlotStackedCL :: (Typeable b, Renderable (Path V2 Double) b,
                  MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                 Int -> [Double] -> [Colour Double]
                     -> [String] -> Double -> m ()
barPlotStackedCL y xs cs ls w = F.for_ (sortBy (compare `on` fstof3)(zip3 (additive xs) cs ls)) $ \x -> barPlotL ((fromIntegral y), fstof3 x) w (trdof3 x) (sndof3 x)

barPlotStackedMultiC :: (Typeable b, Renderable (Path V2 Double) b,
                  MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                  [[Double]] -> [Colour Double]-> [String] -> Double -> m ()
barPlotStackedMultiC xs colormap names w = do 
                                          barPlotStackedCL 1 (xs!!0) colormap names w
                                          F.for_ [2 .. length xs] $ \x -> barPlotStackedC x (xs!!(x-1)) colormap w 

------------------------------------------------------------------------
-- Split Bar
------------------------------------------------------------------------
barPlotSplit :: (Typeable b, Renderable (Path V2 Double) b,
                  MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                 Int -> [Double] -> Double -> m ()
barPlotSplit y xs w = F.for_ (zip (createsplitdata y len w) xs) $ \x -> barPlot x w1
                      where w1 = w/fromIntegral len
                            len = length xs

createsplitdata :: Int -> Int -> Double -> [Double]
createsplitdata y len w =  [b, b+w1 .. (a-w1)]
                           where y1 = fromIntegral y
                                 w1 = w/fromIntegral len
                                 a  = y1+(w/2)+ (w1/2)
                                 b  = y1-(w/2)+ (w1/2)

barPlotSplitC :: (Typeable b, Renderable (Path V2 Double) b,
                  MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                 Int -> [Double] -> [Colour Double]-> Double -> m ()
barPlotSplitC y xs cs w = F.for_ (zip (zip (createsplitdata y len w) xs) cs) $ \x -> barPlotC (fst x) w1 (snd x) 
                         where w1  = w/fromIntegral len
                               len = length xs

barPlotSplitCL :: (Typeable b, Renderable (Path V2 Double) b,
                  MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                 Int -> [Double] -> [Colour Double] 
                     -> [String] -> Double -> m ()
barPlotSplitCL y xs cs ls w = F.for_ (zip3 (zip (createsplitdata y len w) xs) cs ls) $ \x -> barPlotL (fstof3 x) w1 (trdof3 x) (sndof3 x) 
                             where w1  = w/fromIntegral len
                                   len = length xs

barPlotSplitMultiC :: (Typeable b, Renderable (Path V2 Double) b,
                  MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                  [[Double]] -> [Colour Double]-> [String] -> Double -> m ()
barPlotSplitMultiC xs colormap names w = do 
                                         barPlotSplitCL 1 (xs!!0) colormap names w
                                         F.for_ [2 .. length xs] $ \x -> barPlotSplitC x (xs!!(x-1)) colormap w 
------------------------------------------------------------------------
-- Ratio Bar
------------------------------------------------------------------------
barPlotRatio :: (Typeable b, Renderable (Path V2 Double) b,
                  MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                 Int -> [Double] -> Double -> m ()
barPlotRatio t xs w = do 
                      barPlotStacked t [ x/tot | x <- xs] w
                      where tot = sum xs

barPlotRatioC :: (Typeable b, Renderable (Path V2 Double) b,
                  MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                 Int -> [Double] -> [Colour Double] -> Double -> m ()
barPlotRatioC t xs colormap w = do 
                                barPlotStackedC t [x/tot | x <- xs] colormap w
                                where tot = sum xs
                             
barPlotRatioCL :: (Typeable b, Renderable (Path V2 Double) b,
                  MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                 Int -> [Double] -> [Colour Double] 
                     -> [String]-> Double -> m ()
barPlotRatioCL t xs colormap names w = do 
                                       barPlotStackedCL t [x/tot | x <- xs] colormap names w
                                       where tot = sum xs

barPlotRatioMultiC :: (Typeable b, Renderable (Path V2 Double) b,
                  MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                  [[Double]] -> [Colour Double] -> [String] -> Double -> m ()
barPlotRatioMultiC xs colormap names w = do 
                                         barPlotRatioCL 1 (xs!!0) colormap names w
                                         F.for_ [2 .. length xs] $ \x -> barPlotRatioC x (xs!!(x-1)) colormap w 

------------------------------------------------------------------------
-- Histogram
------------------------------------------------------------------------

histogramPlot
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (HistogramPlot v n) b,
      F.Foldable f, Enum n)
  => f p -> m ()
histogramPlot d = addPlotable (mkHistogramPlot d)

histogramPlot'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (HistogramPlot v n) b,
      F.Foldable f, Enum n)
  => f p -> PlotState (HistogramPlot v n) b -> m ()
histogramPlot' d = addPlotable' (mkHistogramPlot d)

histogramPlotL
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (HistogramPlot v n) b,
      F.Foldable f, Enum n)
  => String -> f p -> m ()
histogramPlotL l d = addPlotableL l (mkHistogramPlot d)

-- Fold variants

histogramPlotOf
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (HistogramPlot v n) b, Enum n)
  => Fold s p -> s -> m ()
histogramPlotOf f s = addPlotable (mkHistogramPlotOf f s)

histogramPlotOf'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (HistogramPlot v n) b, Enum n)
  => Fold s p -> s -> PlotState (HistogramPlot v n) b -> m ()
histogramPlotOf' f s = addPlotable' (mkHistogramPlotOf f s)

histogramPlotLOf
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (HistogramPlot v n) b, Enum n)
  => String -> Fold s p -> s -> m ()
histogramPlotLOf l f s = addPlotableL l (mkHistogramPlotOf f s)

------------------------------------------------------------------------
-- Boxplot Vertical --fillalso
------------------------------------------------------------------------

boxplotvPlot :: (Typeable b, Renderable (Path V2 Double) b,
                 MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                (Double, Double) -> Double -> Double -> Double -> m ()
boxplotvPlot (a,b) s1 s2 h = do
                             crossbarhPlot (a,b) s1 h
                             linePlot' (createlinerangeh (a-(s1/2), b) (s2-s1)) $ do
                                 plotColor .= red
                             linePlot' (createlinerangeh (a+(s1/2), b) (s2-s1)) $ do
                                 plotColor .= red


------------------------------------------------------------------------
-- Boxplot Horizontal --fillalso
------------------------------------------------------------------------

boxplothPlot :: (Typeable b, Renderable (Path V2 Double) b,
                 MonadState (Axis b c Double) m, BaseSpace c ~ V2) =>
                (Double, Double) -> Double -> Double -> Double -> m ()
boxplothPlot (a,b) s h1 h2= do
                            crossbarhPlot (a,b) s h1
                            linePlot' (createlinerangeh (a, b1) hmean) $ do
                                 plotColor .= red
                            linePlot' (createlinerangeh (a, b2) hmean) $ do
                                 plotColor .= red
                            where b1 = b + (h1/2) + (h2/2)
                                  b2 = b - (h1/2) - (h2/2)
                                  hmean = h2-h1


-- @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
------------------------------------------------------------------------
-- Bar Plot
------------------------------------------------------------------------

-- $bar
-- Bar plots are different in that you often want to specify the axis
-- name along with the data.

-- barPlot :: (R2Backend, Plotable (Path v n) b, F.Foldable f)
--         => f (String, n) -> AxisState b v n
-- barPlot d = addPlotable $ P.mkBarPlot' d

-- barPlot' :: (R2Backend, Plotable (Path v n) b, F.Foldable f)
--         => f (String, n) -> AxisState b v n
-- barPlot' d s = addPlotable (P.mkBarPlot d) s

-- barPlotL :: (R2Backend, Plotable (Path v n) b, F.Foldable f)
--         => String -> f (String, n) -> AxisState b v n
-- barPlotL s d = addPlotableL s (P.mkBarPlot d)

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
