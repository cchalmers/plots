{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Plots.Types.Bar
-- Copyright   :  (C) 2015 Christopher Chalmers
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Christopher Chalmers
-- Stability   :  experimental
-- Portability :  non-portable
--
-- A bar plot is a plot that presents data with rectangular bars with
-- lengths proportional to the values that they represent. The bars can
-- be plotted vertically or horizontally.
--
-- <<diagrams/src_Plots_Types_Bar_multiBarExample.svg#diagram=multiBarExample&height=350>>
--
-- (see 'multiBars' example for code to make this plot)
--
----------------------------------------------------------------------------
module Plots.Types.Bar
  (
    -- * BarPlot
    BarPlot
  , barPlot
  , barPlot'
  , namedBarPlot
  , namedBarPlot'
  , floatingBarPlot

    -- * Bar layout
  , BarLayout
  , HasBarLayout (..)

    -- * Multi bars
    -- ** Adding to axis
  , multiBars
  , MultiBarState

    -- ** Multi bar types
  , groupedBars
  , groupedBars'
  , stackedBars
  , stackedEqualBars
  , runningBars

    -- ** Modify multi bars
  , onBars
  , labelBars


    -- * Low level constructors
  , mkBars
  , mkFloatingBars
  , mkRunningBars
  , mkStackedBars
  , mkStackedEqualBars
  , mkGroupedBars
  -- , barAxisLabels

  ) where

import           Control.Lens            hiding (at, none, transform, ( # ))
import           Control.Monad.State
import           Data.Typeable
import qualified Data.Foldable as F

import           Plots.Style
import           Plots.Types
import           Plots.Axis
import           Plots.Axis.Ticks
import           Plots.Axis.Labels
import           Plots.Util

import qualified Data.List               as List
-- import           Diagrams.Core.Transform (fromSymmetric)
-- import           Linear.V2               (_yx)

import Geometry.TwoD.Transform

import           Diagrams.Prelude

-- Single bar ----------------------------------------------------------

-- | Data for a single bar. The bar is drawn as
--
-- @
-- fromCorners (V2 barPos (fst barBounds))) (V2 (barPos + barWidth) (snd barBounds))
-- @
--
--   for 'Horizontal' bars, flipped for 'Vertical'. This is a low level
--   representation of a bar and is not intended to be used directly.

-- | Construct a rectangle of size v with the bottom centre at point p.
rectB :: (InSpace V2 n t, Fractional n, FromTrail t) => Point V2 n -> V2 n -> t
rectB p (V2 x y) =
  fromLocTrail $ fromOffsets [V2 x 0, V2 0 y, V2 (-x) 0] # closeTrail `at` p .-^ V2 (x/2) 0

------------------------------------------------------------------------
-- Bar layout
------------------------------------------------------------------------

-- | The way an individual bar plot or a group of bars plots are laid
--   out on the axis.
data BarLayout = BarLayout
  { bOrient  :: Orientation
  , bWidth   :: Double
  , bSpacing :: Double
  , bStart   :: Double
  } deriving Typeable

instance Default BarLayout where
  def = BarLayout Horizontal 0.8 1 1

type instance N BarLayout = Double

instance HasOrientation BarLayout where
  orientation = lens bOrient (\bl o -> bl {bOrient = o})

-- | Class of things that have a modifiable 'BarLayout'.
class HasOrientation a => HasBarLayout a where
  -- | Lens onto the 'BarLayout'
  barLayout :: Lens' a BarLayout

  -- | The width bar for single / stacked bars or the width of a group
  --   for grouped bar plot.
  --
  --   Default is @0.8@
  barWidth :: Lens' a Double
  barWidth = barLayout . lens bWidth (\bl w -> bl {bWidth = w})

  -- | The spacing between each bar or group of bars.
  --
  --   Default is @1@
  barSpacing :: Lens' a Double
  barSpacing = barLayout . lens bSpacing (\bl s -> bl {bSpacing = s})

  -- | The distance from the axis to centre of the first bar.
  --
  --   Default is @1@
  barStart :: Lens' a Double
  barStart = barLayout . lens bStart (\bl x -> bl {bStart = x})

instance HasBarLayout BarLayout where
  barLayout = id

instance HasBarLayout a => HasBarLayout (Plot a) where
  barLayout = rawPlot . barLayout

------------------------------------------------------------------------
-- Bar plot type
------------------------------------------------------------------------

-- | A bar plot for a single set of bars. Multi-bar plots are achieved
--   by having multiple 'BarPlot's. Each bar plot corresponds to a
--   single legend entry. To get multiple bar entries/colours, use
--   multiple 'BarPlots'

--   A 'BarPlot' is not intended to be constructed directly, instead use
--   one of the helper functions.
data BarPlot = BarPlot
  { bpData   :: [(Double,Double)]
  , bpLayout :: BarLayout
  } deriving Typeable

type instance V BarPlot = V2
type instance N BarPlot = Double

instance HasOrientation BarPlot where
  orientation = barLayout . orientation

instance Enveloped BarPlot where
  getEnvelope BarPlot {..} =
    getEnvelope . orient bpLayout _reflectXY id . (id :: Path v n -> Path v n) $
      ifoldMap drawBar bpData
    where
      drawBar i (a,b) = rectB (mkP2 x a) (V2 (view barWidth bpLayout) (b - a))
        where x = view barStart bpLayout + fromIntegral i * view barSpacing bpLayout

instance Plotable BarPlot where
  renderPlotable s sty BarPlot {..} =
    ifoldMap drawBar bpData
      # orient bpLayout _reflectXY id
      # applyAreaStyle sty
      # transform (s^.specTrans)
    where
      drawBar i (a,b) = rectB (mkP2 x a) (V2 (view barWidth bpLayout) (b - a))
        where x = view barStart bpLayout + fromIntegral i * view barSpacing bpLayout

  defLegendPic sty BarPlot {..}
    = centerXY
    . applyAreaStyle sty'
    . orient bpLayout _reflectXY id
    $ d
    where
      -- Multiple bars get two bars next to each other for the legend. A
      -- single bar only gets one bar in the legend.
      d | has (ix 1) bpData = alignB (rect 4 7) ||| strutX 3 ||| alignB (rect 4 10)
        | otherwise         = rect 4 10

      -- The legend bars don't look right if the line width is too big so we limit it
      sty' = sty & areaStyle . _lw . mapped %~ atMost (local 0.8)

instance HasBarLayout BarPlot where
  barLayout = lens bpLayout (\bp l -> bp {bpLayout = l})

------------------------------------------------------------------------
-- Constructing bar plots
------------------------------------------------------------------------

-- | Create equidistant bars using the values.
mkBars :: F.Foldable f => BarLayout -> f Double -> BarPlot
mkBars bl (F.toList -> ns) = mkFloatingBars bl (map (0,) ns)

-- | Create equidistant bars with lower and upper bounds for each bar.
mkFloatingBars :: F.Foldable f => BarLayout -> f (Double,Double) -> BarPlot
mkFloatingBars bl (F.toList -> ns) = BarPlot
  { bpData   = ns
  , bpLayout = bl
  }

-- | Create uniform bars from groups of data, placing one group after
--   the other.
mkRunningBars
  :: BarLayout
  -> [[(Double, Double)]]
  -> [BarPlot]
mkRunningBars bl = snd . foldr f (view barStart bl, [])
  where
    f d (x, bs) = (x + dx, mkFloatingBars bl {bStart = x} d : bs)
      where dx = view barSpacing bl * fromIntegral (length d)

-- | Create uniform bars from groups of data, placing one on top of the
--   other. The first list will be the same as @mkUniformBars opts (map
--   (0,) ys)@, subsequent lists will be placed on top.
mkStackedBars
  :: BarLayout
  -> [[Double]] -- values
  -> [BarPlot]
mkStackedBars bl = snd . List.mapAccumR f (repeat 0)
  where
    -- y0s are the base values for this set of bars, these accumulate
    -- for each set of data
    f y0s ys = (y1s, mkFloatingBars bl ds)
      where y1s  = liftU2 (+) y0s ys
            ds   = zipWith (\y0 y -> (y0, y0 + y)) y0s ys

-- | Similar to 'mkMultiStacked' but stack has the same height.
mkStackedEqualBars
  :: Double     -- ^ value each bar reaches
  -> BarLayout
  -> [[Double]] -- ^ values
  -> [BarPlot]
mkStackedEqualBars yM bl yss = mkStackedBars bl yss'
  where
    -- Multiplier for each bar to reach the desired height.
    ms = map (\ys -> yM / sum ys) $ List.transpose yss

    -- Normalise each data set by multiplying it with the normalising
    -- factor.
    yss' = map (zipWith (*) ms) yss

-- | Make bars that are grouped together. Each group of bars is treated
--   as a single bar when using the 'BarPlotsOpts'. There is an addition
--   parameter to adjust the width of each individual bar.
mkGroupedBars
  :: Double -- ^ width factor of individual bars (1 = touching)
  -> BarLayout
  -> [[Double]]
  -> [BarPlot]
mkGroupedBars w bl xs =
  flip imap xs $ \i ns ->
    mkBars
      bl { bStart = start' + width' * fromIntegral i
         , bWidth = width' * w
         }
      ns
  where
    n = fromIntegral $ length xs
    -- start' is such that middle of the middle bar is now at
    -- barOptsStart bo
    start' = bStart bl - (n - 1) * width' / 2
    width' = bWidth bl / n

-- temporary functions that will be in next lib release

_reflectionXY :: (HasLinearMap v, R2 v, Num n) => Transformation v n
_reflectionXY = reflectionX <> reflectionY

_reflectXY :: (InSpace v n t, R2 v, HasLinearMap v, Transformable t) => t -> t
_reflectXY = transform _reflectionXY

----------------------------------------------------------------------------------
-- Single bar state API
----------------------------------------------------------------------------------

-- | A add 'BarPlot' to an 'Axis'.
--
-- === __Example__
--
-- <<diagrams/src_Plots_Types_Bar_barExample.svg#diagram=barExample&height=400>>
--
-- > import Plots
-- > barAxis :: Axis B V2 Double
-- > barAxis = r2Axis &~ do
-- >   yMin ?= 0
-- >   hide majorGridLines
-- >   barPlot [13.5, 3.0, 6.9, 7.2, 4.6] $ do
-- >     vertical .= True
-- >     barWidth //= 2
--
-- > barExample = renderAxis barAxis
barPlot
  :: (MonadState (Axis V2) m, F.Foldable f)
  => f Double                      -- ^ bar heights
  -> State (Plot BarPlot) () -- ^ changes to the bars
  -> m ()                          -- ^ changes to the 'Axis'
barPlot ns = addPlotable (mkBars def ns)

-- | Simple version of 'barPlot' without any modification to the 'Plot'.
--
-- === __Example__
--
-- <<diagrams/src_Plots_Types_Bar_barExample'.svg#diagram=barExample'&height=400>>
--
-- > import Plots
-- > barAxis' :: Axis B V2 Double
-- > barAxis' = r2Axis &~ do
-- >   xMin ?= 0
-- >   hide (yAxis . majorGridLines)
-- >   barPlot' [13.5, 3.0, 6.9, 7.2, 4.6]
--
-- > barExample' = renderAxis barAxis'
barPlot'
  :: (MonadState (Axis V2) m, F.Foldable f)
  => f Double  -- ^ bar heights
  -> m ()      -- ^ changes to the 'Axis'
barPlot' ns = addPlotable' (mkBars def ns)

-- | A add 'BarPlot' to an 'Axis' while naming the bars.
--
-- === __Example__
--
-- <<diagrams/src_Plots_Types_Bar_namedBarExample.svg#diagram=namedBarExample&height=400>>
--
-- > import Plots
-- > namedBarAxis :: Axis B V2 Double
-- > namedBarAxis = r2Axis &~ do
-- >   yMin ?= 0
-- >   hide (xAxis . majorGridLines)
-- >   namedBarPlot [("eggs", 12), ("bacon", 5), ("sausage", 9), ("beans", 3)] $ do
-- >     vertical .= True
-- >     barWidth //= 2
-- >
-- > namedBarExample = renderAxis namedBarAxis
namedBarPlot
  :: (MonadState (Axis V2) m, F.Foldable f)
  => f (String, Double)                  -- ^ bar heights with name
  -> State (Plot BarPlot) () -- ^ changes to the bars
  -> m ()                          -- ^ changes to the 'Axis'
namedBarPlot d s = do
  addPlot bp
  barLayoutAxisLabels (bp ^. barLayout) nms
  where
    (nms, xs) = unzip $ F.toList d
    bp = mkPlot (mkBars def xs) & execState s

-- | Simple version of 'namedBarPlot' without any modification to the 'Plot'.
--
-- === __Example__
--
-- <<diagrams/src_Plots_Types_Bar_namedBarExample'.svg#diagram=namedBarExample'&height=400>>
--
-- > import Plots
-- > namedBarAxis' :: Axis B V2 Double
-- > namedBarAxis' = r2Axis &~ do
-- >   xMin ?= 0
-- >   hide majorGridLines
-- >   namedBarPlot' [("eggs", 12), ("bacon", 5), ("sausage", 9), ("beans", 3)]
--
-- > namedBarExample' = renderAxis namedBarAxis'
namedBarPlot'
  :: (MonadState (Axis V2) m, F.Foldable f)
  => f (String, Double) -- ^ bar heights with name
  -> m ()               -- ^ add plot to the 'Axis'
namedBarPlot' ns = namedBarPlot ns (return ())

-- | Same as 'barPlot' but with lower and upper bounds for the bars.
floatingBarPlot
  :: (MonadState (Axis V2) m, F.Foldable f)
  => f (Double,Double) -- ^ bar limits
  -> State (Plot BarPlot) () -- ^ changes to the bars
  -> m ()
floatingBarPlot ns = addPlotable (mkFloatingBars def ns)

------------------------------------------------------------------------
-- Multi bar state API
------------------------------------------------------------------------

-- Multi bar state -----------------------------------------------------

-- | The 'MultiBarState' is used to set the various options available
--   when building multiple bar plots together. The main functions used
--   to modify this state:
--
--   * To choose the way the bars are grouped together choose one of
--
--       * 'groupedBars'      - Together in grouped (the default)
--       * 'stackedBars'      - On on top of another
--       * 'stackedEqualBars' - 'stackedBars' with the same height
--       * 'runningBars'      - each group of bars follows the last
--
--   * Modify the 'PlotOptions' and 'PlotStyle' of groups of bars with
--     'onBars'.
--
--   * Modify the layout of the (groups of) bars with
--
--       * 'orientation' - Horizontal or vertical bars
--       * 'barWidth'    - Width of each (group of) bar(s)
--       * 'barSpacing'  - Space between each (group of) bar(s)
--       * 'barStart'    - Start of centre of first bar
--
--   * Add labels to each (group of) bars with 'labelBars'.
--
data MultiBarState a = MultiBarState
  { mbsLayout :: BarLayout
    -- ^ options for building bar plots

  , mbsMods :: [(a, Endo (PlotMods V2))]
    -- ^ the data along with an adjustment to the plot properties

  , mbsLabels  :: [String]
    -- ^ labels to be placed at the bottom of each bar

  , mbsBarFun :: BarLayout -> [[Double]] -> [BarPlot]
    -- ^ function used to build bar plots
  }

type instance N (MultiBarState a) = Double

instance HasOrientation (MultiBarState a) where
  orientation = barLayout . orientation

instance HasBarLayout (MultiBarState a) where
  barLayout = lens mbsLayout (\mbs l -> mbs {mbsLayout = l})

-- > import Plots
--
-- > groupedExample s = r2Axis &~ do
-- >   yMin ?= 0
-- >   hide (xAxis . majorGridLines)
-- >   xLabel .= "breakfast item"
-- >   hide minorTicks
-- >   multiBars sortedData snd $ do
-- >     vertical .= True
-- >     barWidth //= 2
-- >     labelBars (map fst breakfastData)
-- >     onBars $ \(nm,_) -> key nm
-- >     s
-- >
-- >   -- show y values without decimal point
-- >   yAxis . tickLabelFunction .= atMajorTicks (show . round)
-- >   -- we should really force all major ticks to like on integers too
--
-- > groupedBarsExample  = renderAxis $ groupedExample groupedBars
-- > groupedBarsExample' = renderAxis $ groupedExample (groupedBars' 0.7)
-- > stackedBarsExample  = renderAxis $ groupedExample  stackedBars
-- > stackedEqualBarsExample = renderAxis $ groupedExample (stackedEqualBars 10)
-- > runningBarsExample  = renderAxis $ groupedExample $ do
-- >   runningBars
-- >   labelBars (map fst breakfastData ++ map fst breakfastData)

-- Adding to axis ------------------------------------------------------

multiFun :: Lens' (MultiBarState a) (BarLayout -> [[Double]] -> [BarPlot])
multiFun = lens mbsBarFun (\mbs f -> mbs {mbsBarFun = f})

-- | Bars that are grouped together such that each group is a single
--   'barWidth'. The bars in a group are touching, see groupedBars' to
--   reduce the width of individual bars.
--
-- === __Example__
--
-- <<diagrams/src_Plots_Types_Bar_groupedBarsExample.svg#diagram=groupedBarsExample&height=400>>

groupedBars :: State (MultiBarState a) ()
groupedBars = groupedBars' 1

-- | Bars that are grouped together such that each group is a single
--   'barWidth'. The parameter is the multiplier for the width of
--   individual bars, where @'groupedBars' 1 = groupedBars@ corresponds
--   to bars in a group touching. reduce the width of individual bars.
--
-- === __Example__
--
-- <<diagrams/src_Plots_Types_Bar_groupedBarsExample'.svg#diagram=groupedBarsExample'&height=400>>
--
groupedBars' :: Double -> State (MultiBarState a) ()
groupedBars' n = multiFun .= mkGroupedBars n

-- | Bars stacked on top of each other.
--
-- === __Example__
--
-- <<diagrams/src_Plots_Types_Bar_stackedBarsExample.svg#diagram=stackedBarsExample&height=400>>
--
stackedBars :: State (MultiBarState a) ()
stackedBars = multiFun .= mkStackedBars

-- | Bars stacked on top of each other where every bar is the given
--   height.
--
--
-- === __Example__
--
-- <<diagrams/src_Plots_Types_Bar_stackedEqualBarsExample.svg#diagram=stackedEqualBarsExample&height=400>>
--
stackedEqualBars :: Double -> State (MultiBarState a) ()
stackedEqualBars n = multiFun .= mkStackedEqualBars n

-- | Normal 'bars' where each data set follows the last.
--
-- === __Example__
--
-- <<diagrams/src_Plots_Types_Bar_runningBarsExample.svg#diagram=runningBarsExample&height=400>>
--
runningBars :: State (MultiBarState a) ()
runningBars = multiFun .= \l xs -> mkRunningBars l (map (map (0,)) xs)

-- | Construct multiple bars, grouped together. See 'MultiBarState' for
--   details on how to customise how the bars are drawn.
--
-- === __Example__
--
-- <<diagrams/src_Plots_Types_Bar_multiBarExample.svg#diagram=multiBarExample&height=400>>
--
-- > import Plots
-- > breakfastData :: [(String, V2 Double)]
-- > breakfastData = [("eggs", V2 7 5), ("bacon", V2 5 4), ("sausage", V2 2 7), ("beans", V2 2 1)]
--
-- > sortedData = [ ("girls", breakfastData^..each._2._x)
-- >              , ("boys",  breakfastData^..each._2._y)
-- >              ]
--
-- > multiBarAxis :: Axis B V2 Double
-- > multiBarAxis = r2Axis &~ do
-- >   yMin ?= 0
-- >   hide (xAxis . majorGridLines)
-- >   hide minorTicks
-- >   xLabel .= "breakfast item"
-- >   multiBars sortedData snd $ do
-- >     vertical .= True
-- >     barWidth //= 2
-- >     labelBars (map fst breakfastData)
-- >     onBars $ \(nm,_) -> key nm
-- >
-- >   -- show y values without decimal point
-- >   yAxis . tickLabelFunction .= atMajorTicks (show . round)
-- >   -- we should really force all major ticks to like on integers too
--
-- > multiBarExample = renderAxis multiBarAxis
multiBars
  :: (MonadState (Axis V2) m, F.Foldable f, F.Foldable g)
  => f a                            -- ^ data for multi plot
  -> (a -> g Double)                -- ^ extract bar heights from each data set
  -> State (MultiBarState a) () -- ^ state to make changes to the plot
  -> m ()                           -- ^ changes to the 'Axis'
multiBars (F.toList -> as) f st = do
  -- add the plots
  F.forM_ propertiedBars $ \(b,endo) ->
    addPlotable b $ plotMods %= appEndo endo

  -- label bars on axis if necessary
  barLayoutAxisLabels (bs ^. barLayout) (bs ^. labels)
  where
    -- bars
    propertiedBars = zip barPlots endos
    barPlots = mbsBarFun bs (bs ^. barLayout) $ map (F.toList . f) as

    -- data and modifiers
    endos = mbsMods bs ^.. each . _2

    -- bar state
    bs  = execState st bs0
    bs0 = MultiBarState
      { mbsLayout = def
      , mbsMods   = map (\a -> (a, mempty)) as
      , mbsLabels = []
      , mbsBarFun = mkGroupedBars 1
      }

-- | Place labels under the centre of each bar using the 'BarLayout' by
--   changing that 'axisTickLabels', using the provided string in order.
barLayoutAxisLabels :: MonadState (Axis V2) m => BarLayout -> [String] -> m ()
barLayoutAxisLabels bl ls =
  unless (null ls) $
    axes . orient bl _y _x &= do
      majorTickPositions  .= xs
      minorTicks . visible .= False
      tickLabelPositions   .= zip xs ls
  where
    xs = map ((+ view barStart bl) . (* view barSpacing bl) . fromIntegral)
             [0 .. length ls - 1]

-- | Given the data for the bar, modify the properties for the bar that
--   uses that data.
--
--   Some common functions to use on the 'PlotMods':
--
--       * 'plotColour' - change the colour of the bars
--
--       * 'areaStyle' - modify the style of the bars
--
--       * 'key' - add a legend entry for that group of bars
--
onBars
  :: (a -> State (PlotMods V2) ())
     -- ^ Modifier the 'PlotOptions' and 'PlotStyle' for the bars
     -- associated with the data from @a@.

  -> State (MultiBarState a) ()
     -- ^ Changes to each data set when executing 'multiBars'.
onBars f =
  mods . mapped %= \(a, endo) -> (a, endo <> Endo (execState (f a)))
  where mods = lens mbsMods (\bs d -> bs {mbsMods = d})

class HasLabels a where
  labels :: Lens' a [String]

instance HasLabels (MultiBarState a) where
  labels = lens mbsLabels (\mbs ls -> mbs {mbsLabels = ls})

-- | Labels to use for each bar (or group of bars) along the axis.
labelBars :: HasLabels a => [String] -> State a ()
labelBars xs = labels .= xs
