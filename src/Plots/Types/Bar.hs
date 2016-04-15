{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
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
import           Plots.Utils

import qualified Data.List               as List
import           Diagrams.Core.Transform (fromSymmetric)
import           Linear.V2               (_yx)

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
rectB :: (InSpace V2 n t, TrailLike t) => Point V2 n -> V2 n -> t
rectB p (V2 x y) =
  trailLike $ fromOffsets [V2 x 0, V2 0 y, V2 (-x) 0] # closeTrail `at` p .-^ V2 (x/2) 0

------------------------------------------------------------------------
-- Bar layout
------------------------------------------------------------------------

-- | The way an individual bar plot or a group of bars plots are laid
--   out on the axis.
data BarLayout n = BarLayout
  { bOrient  :: Orientation
  , bWidth   :: n
  , bSpacing :: n
  , bStart   :: n
  }

makeLensesFor [ ("bOrient",  "_barOrient")
              , ("bWidth",   "_barWidth")
              , ("bSpacing", "_barSpacing")
              , ("bStart",   "_barStart") ] ''BarLayout

instance Fractional n => Default (BarLayout n) where
  def = BarLayout Horizontal 0.8 1 1

type instance N (BarLayout n) = n

instance HasOrientation (BarLayout n) where
  orientation = _barOrient

-- | Class of things that have a modifiable 'BarLayout'.
class HasOrientation a => HasBarLayout a where
  -- | Lens onto the 'BarLayout'
  barLayout :: Lens' a (BarLayout (N a))

  -- | The width bar for single / stacked bars or the width of a group
  --   for grouped bar plot.
  --
  --   Default is @0.8@
  barWidth :: Lens' a (N a)
  barWidth = barLayout . _barWidth

  -- | The spacing between each bar or group of bars.
  --
  --   Default is @1@
  barSpacing :: Lens' a (N a)
  barSpacing = barLayout . _barSpacing

  -- | The distance from the axis to centre of the first bar.
  --
  --   Default is @1@
  barStart :: Lens' a (N a)
  barStart = barLayout . _barStart

instance HasBarLayout (BarLayout n) where
  barLayout = id

instance HasBarLayout a => HasBarLayout (Plot a b) where
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
data BarPlot n = BarPlot
  { bpData   :: [(n,n)]
  , bpLayout :: BarLayout n
  }

type instance V (BarPlot n) = V2
type instance N (BarPlot n) = n

instance HasOrientation (BarPlot n) where
  orientation = barLayout . orientation

instance OrderedField n => Enveloped (BarPlot n) where
  getEnvelope BarPlot {..} =
    getEnvelope . orient bpLayout _reflectXY id . (id :: Path v n -> Path v n) $
      ifoldMap drawBar bpData
    where
      drawBar i (a,b) = rectB (mkP2 x a) (V2 (view barWidth bpLayout) (b - a))
        where x = view barStart bpLayout + fromIntegral i * view barSpacing bpLayout

instance (Typeable b, TypeableFloat n, Renderable (Path V2 n) b)
    => Plotable (BarPlot n) b where
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
      sty' = sty & areaStyle . _lw %~ atMost (local 0.8)

instance HasBarLayout (BarPlot n) where
  barLayout = lens bpLayout (\bp l -> bp {bpLayout = l})

------------------------------------------------------------------------
-- Constructing bar plots
------------------------------------------------------------------------

-- | Create equidistant bars using the values.
mkBars :: (Foldable f, Num n) => BarLayout n -> f n -> BarPlot n
mkBars bl (F.toList -> ns) = mkFloatingBars bl (map (0,) ns)

-- | Create equidistant bars with lower and upper bounds for each bar.
mkFloatingBars :: (Foldable f, Num n) => BarLayout n -> f (n,n) -> BarPlot n
mkFloatingBars bl (F.toList -> ns) = BarPlot
  { bpData   = ns
  , bpLayout = bl
  }

-- | Create uniform bars from groups of data, placing one group after
--   the other.
mkRunningBars
  :: Num n
  => BarLayout n
  -> [[(n,n)]]
  -> [BarPlot n]
mkRunningBars bl = snd . foldr f (view barStart bl, [])
  where
    f d (x, bs) = (x + dx, mkFloatingBars bl {bStart = x} d : bs)
      where dx = view barSpacing bl * fromIntegral (length d)

-- | Create uniform bars from groups of data, placing one on top of the
--   other. The first list will be the same as @mkUniformBars opts (map
--   (0,) ys)@, subsequent lists will be placed on top.
mkStackedBars
  :: Num n
  => BarLayout n
  -> [[n]] -- values
  -> [BarPlot n]
mkStackedBars bl = snd . List.mapAccumR f (repeat 0)
  where
    -- y0s are the base values for this set of bars, these accumulate
    -- for each set of data
    f y0s ys = (y1s, mkFloatingBars bl ds)
      where y1s  = liftU2 (+) y0s ys
            ds   = zipWith (\y0 y -> (y0, y0 + y)) y0s ys

-- | Similar to 'mkMultiStacked' but stack has the same height.
mkStackedEqualBars
  :: Fractional n
  => n     -- ^ value each bar reaches
  -> BarLayout n
  -> [[n]] -- ^ values
  -> [BarPlot n]
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
  :: Fractional n
  => n -- ^ width factor of individual bars (1 = touching)
  -> BarLayout n
  -> [[n]]
  -> [BarPlot n]
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

_reflectionXY :: (Additive v, R2 v, Num n) => Transformation v n
_reflectionXY = fromSymmetric $ (_xy %~ view _yx) <-> (_xy %~ view _yx)

_reflectXY :: (InSpace v n t, R2 v, Transformable t) => t -> t
_reflectXY = transform _reflectionXY

----------------------------------------------------------------------------------
-- Single bar state API
----------------------------------------------------------------------------------

-- | A add 'BarPlot' to an 'Axis'.
barPlot
  :: (MonadState (Axis b V2 n) m,
      Plotable (BarPlot n) b,
      Foldable f)
  => f n                           -- ^ bar heights
  -> State (Plot (BarPlot n) b) () -- ^ changes to the bars
  -> m ()                          -- ^ changes to the 'Axis'
barPlot ns = addPlotable (mkBars def ns)

-- | Simple version of 'barPlot' without any modification to the 'Plot'.
barPlot'
  :: (MonadState (Axis b V2 n) m,
      Plotable (BarPlot n) b,
      Foldable f)
  => f n  -- ^ bar heights
  -> m () -- ^ changes to the 'Axis'
barPlot' ns = addPlotable' (mkBars def ns)

-- | A add 'BarPlot' to an 'Axis' while naming the bars.
namedBarPlot
  :: (MonadState (Axis b V2 n) m,
      Plotable (BarPlot n) b,
      Foldable f)
  => f (String,n)                  -- ^ bar heights with name
  -> State (Plot (BarPlot n) b) () -- ^ changes to the bars
  -> m ()                          -- ^ changes to the 'Axis'
namedBarPlot d s = do
  addPlot bp
  barLayoutAxisLabels (bp ^. barLayout) nms
  where
    (nms, xs) = unzip $ F.toList d
    bp = mkPlot (mkBars def xs) & execState s

-- | Simple version of 'namedBarPlot' without any modification to the 'Plot'.
namedBarPlot'
  :: (MonadState (Axis b V2 n) m,
      Plotable (BarPlot n) b,
      Foldable f)
  => f (String,n)  -- ^ bar heights with name
  -> m ()          -- ^ add plot to the 'Axis'
namedBarPlot' ns = namedBarPlot ns (return ())

-- | Same as 'barPlot' but with lower and upper bounds for the bars.
floatingBarPlot
  :: (MonadState (Axis b V2 n) m,
      Plotable (BarPlot n) b,
      Foldable f)
  => f (n,n) -- ^ bar limits
  -> State (Plot (BarPlot n) b) () -- ^ changes to the bars
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
data MultiBarState b n a = MultiBarState
  { mbsLayout :: BarLayout n
    -- ^ options for building bar plots

  , mbsMods :: [(a, Endo (PlotMods b V2 n))]
    -- ^ the data along with an adjustment to the plot properties

  , mbsLabels  :: [String]
    -- ^ labels to be placed at the bottom of each bar

  , mbsBarFun :: BarLayout n -> [[n]] -> [BarPlot n]
    -- ^ function used to build bar plots
  }

type instance N (MultiBarState b n a) = n

instance HasOrientation (MultiBarState b n a) where
  orientation = barLayout . orientation

instance HasBarLayout (MultiBarState b n a) where
  barLayout = lens mbsLayout (\mbs l -> mbs {mbsLayout = l})

-- Adding to axis ------------------------------------------------------

multiFun :: Lens' (MultiBarState b n a) (BarLayout n -> [[n]] -> [BarPlot n])
multiFun = lens mbsBarFun (\mbs f -> mbs {mbsBarFun = f})

-- | Bars that are grouped together such that each group is a single
--   'barWidth'. The bars in a group are touching, see groupedBars' to
--   reduce the width of individual bars.
groupedBars :: Fractional n => State (MultiBarState b n a) ()
groupedBars = groupedBars' 1

-- | Bars that are grouped together such that each group is a single
--   'barWidth'. The parameter is the multiplier for the width of
--   individual bars, where @'groupedBars' 1 = groupedBars@ corresponds
--   to bars in a group touching. reduce the width of individual bars.
groupedBars' :: Fractional n => n -> State (MultiBarState b n a) ()
groupedBars' n = multiFun .= mkGroupedBars n

-- | Bars stacked on top of each other.
stackedBars :: Num n => State (MultiBarState b n a) ()
stackedBars = multiFun .= mkStackedBars

-- | Bars stacked on top of each other where every bar is the given
--   height.
stackedEqualBars :: Fractional n => n -> State (MultiBarState b n a) ()
stackedEqualBars n = multiFun .= mkStackedEqualBars n

-- | Normal 'bars' where each data set follows the last.
runningBars :: Num n => State (MultiBarState b n a) ()
runningBars = multiFun .= \l xs -> mkRunningBars l (map (map (0,)) xs)

-- | Construct multiple bars, grouped together. See 'MultiBarState' for
--   details on how to customise how the bars are drawn.
--
multiBars
  :: (MonadState (Axis b V2 n) m,
      Plotable (BarPlot n) b,
      Foldable f,
      Foldable g)
  => f a                            -- ^ data for multi plot
  -> (a -> g n)                     -- ^ extract bar heights from each data set
  -> State (MultiBarState b n a) () -- ^ state to make changes to the plot
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
barLayoutAxisLabels
  :: (MonadState (Axis b V2 n) m, Fractional n)
  => BarLayout n -> [String] -> m ()
barLayoutAxisLabels bl ls =
  unless (null ls) $
    axes . orient bl _y _x &= do
      majorTickPositions .= xs
      minorTickVisible   .= False
      tickLabelPositions .= zip xs ls
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
  :: (a -> State (PlotMods b V2 n) ())
     -- ^ Modifier the 'PlotOptions' and 'PlotStyle' for the bars
     -- associated with the data from @a@.

  -> State (MultiBarState b n a) ()
     -- ^ Changes to each data set when executing 'multiBars'.
onBars f =
  mods . mapped %= \(a, endo) -> (a, endo <> Endo (execState (f a)))
  where mods = lens mbsMods (\bs d -> bs {mbsMods = d})

class HasLabels a where
  labels :: Lens' a [String]

instance HasLabels (MultiBarState b n a) where
  labels = lens mbsLabels (\mbs ls -> mbs {mbsLabels = ls})

-- | Labels to use for each bar (or group of bars) along the axis.
labelBars :: HasLabels a => [String] -> State a ()
labelBars xs = labels .= xs
