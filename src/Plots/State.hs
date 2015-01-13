{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Plots.State
  ( AxisState
  , axisState
  , P.Axis
  , P.r2Axis
  , P.renderAxis
  , P.Plot (..)

    -- * Plotable
  , addPlotable
  , addPlotable'
  , addPlotableL

    -- ** Scatter plot
  , scatterPlot
  , scatterPlot'
  -- , scatterPlotL
  -- , scatterPlotL'

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
  , withLegend

    -- ** Ticks

  -- , ticks

    -- ** Grid lines

  , recentProps
  , cartesianLabels

    -- ** Axis labels
    -- ** Axis title
  ) where

import qualified Plots as P
import Plots.Axis
import Plots.Types
import Diagrams.Coordinates.Isomorphic
import Diagrams.Prelude
import Diagrams.TwoD.Text
import Data.Typeable
import Data.Foldable

import Control.Lens
import Control.Applicative
import Linear
import Control.Monad.State
import Data.Default

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


newtype AxisStateM b v n a = AxisState (State (P.Axis b v n) a)
  deriving (Functor, Applicative, Monad, MonadState (P.Axis b v n))

type AxisState b v n = AxisStateM b v n ()

cartesianLabels :: Traversable v => AxisState b v n
cartesianLabels =
  partsOf (axisLabels . traversed . P.axisLabelText) .= ["x", "y", "z"]

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

addPlotable :: (InSpace v n a, Default (PlotProperties b v n), Plotable a b,
                Typeable n, Typeable b, Typeable v)
            => a -> AxisState b v n
addPlotable a = axisPlots <>= [Plot a def]


-- | Add something 'Plotable' while adjusting the 'PlotProperties' for
--   that plot.
--
-- @
-- myaxis = r2Axis ~& do
--   addPlotable' (pentagon 3) $ do
--     plotName .= mkName 5
--     addLegendEntry "pentagon"
-- @
addPlotable' :: (InSpace v n a, Default (PlotProperties b v n), Plotable a b,
                Typeable n, Typeable b, Typeable v)
            => a -> State (PlotProperties b v n) c -> AxisState b v n
addPlotable' a s = axisPlots <>= [Plot a (execState s def)]

addPlotableL :: (InSpace v n a, Default (PlotProperties b v n), Plotable a b,
                Typeable n, Typeable b, Typeable v)
            => String -> a -> AxisState b v n
addPlotableL l a = axisPlots <>= [Plot a (def & legendEntries <>~ [mkLegendEntry l])]

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
--              scatterPlot data1
--   @
scatterPlot :: (PointLike v n p, R2Backend b n, Plotable (P.ScatterPlot v n) b, Foldable f)
            => f p -> AxisState b v n
scatterPlot d = axisPlots <>= [P.Plot (P.mkScatterPlot d) def]

-- | Make a 'ScatterPlot' and take a 'State' on the plot to alter it's
--   options
--
--   @
--   myaxis = r2Axis &~ do
--              scatterPlot' pointData1 $ do
--                connectingLine .= True
--   @
scatterPlot' :: (PointLike v n p, R2Backend b n, Plotable (P.ScatterPlot v n) b, Foldable f)
            => f p -> State (P.ScatterPlot v n) a -> AxisState b v n
scatterPlot' d s = axisPlots <>= [P.Plot (execState s $ P.mkScatterPlot d) def]

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
linePlot d = axisPlots <>= [P.Plot (P.mkPath $ Identity d) def]

pathPlot :: (R2Backend b n) => Path V2 n -> AxisState b V2 n
pathPlot p = axisPlots <>= [Plot p def]

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
              => QDiagram b v n Any -> Plot b v n
diagramPlot d = Plot d def

------------------------------------------------------------------------
-- Axis lenses
------------------------------------------------------------------------


-- | Traversal over the axis' most recent 'PlotProperties'.
recentProps :: Traversal' (Axis b v n) (PlotProperties b v n)
recentProps = axisPlots . _last . plotProperties

withLegend :: Num n => String -> AxisState b v n
withLegend s = recentProps . legendEntries <>= [mkLegendEntry s]

axisState :: Axis b v n -> AxisStateM b v n a -> Axis b v n
axisState a (AxisState s) = execState s a

noGridLines :: Functor v => AxisState b v n
noGridLines = modify P.noGridLines

