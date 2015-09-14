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

module Plots.Types.Bar
  (
    -- * BarPlot
    BarPlot (..)

    -- * Helpers
  , bars
  , bars'
  , barRunning
  , barRunning'
  , barStacked
  , barStacked'
  , barGrouped
  , barGrouped'
  , barStackedEqual
  , barStackedEqual'

    -- * Bar state
  , labelBars

    -- ** Multi-bar plots
  , modifyBars

    -- ** Options

  , BarOptions
  , barWidth
  , barSpacing
  , barStart
  , barIndividualWidth

    -- * Low level constructors
  , mkUniformBars
  , mkMultiRunning
  , mkMultiStacked
  , mkMultiStackedEqual
  , mkGrouped
  , barAxisLabels

  -- * Internals
  , ABar (..)
  , HasLabels (..)
  , HasBarOptions (..)

  ) where

import           Control.Lens            hiding (at, none, transform, ( # ))
import           Control.Monad.State
import           Data.Typeable
import qualified Data.Foldable as F

import           Plots.Style
import           Plots.Types
import Plots.Axis
import Plots.API

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
data ABar n = ABar
  { aBarPos    :: n     -- ^ distance from origin along orientation
  , aBarBounds :: (n,n) -- ^ start and finish of data
  , aBarWidth  :: n     -- ^ width of bar
  } deriving (Show, Typeable, Functor)

-- | Construct a rectangle of size v with the bottom centre at point p.
rectB :: (InSpace V2 n t, TrailLike t) => Point V2 n -> V2 n -> t
rectB p (V2 x y) =
  trailLike $ fromOffsets [V2 x 0, V2 0 y, V2 (-x) 0] # closeTrail `at` p .-^ V2 (x/2) 0

-- | Draw a single vertical bar. Use 'reflectXY' to make it a horizontal
--   bar.
drawBar :: (InSpace V2 n t, TrailLike t) => ABar n -> t
drawBar ABar {..}   =
  rectB (mkP2 aBarPos   (uncurry min aBarBounds))
        (V2   aBarWidth (abs $ uncurry (-) aBarBounds))

-- Multiple bars -------------------------------------------------------

-- | A bar plot for a single set of bars. Multi-bar plots are achieved
--   by having multiple 'BarPlot's. Each bar plot corresponds to a
--   single legend entry. To get multiple bar entries/colours, use
--   multiple 'BarPlots'

--   A 'BarPlot' is not intended to be constructed directly, instead use
--   one of the helper functions.
data BarPlot n = BarPlot
  { bData   :: [ABar n]
  , bOrient :: Orientation
  } deriving (Typeable, Functor)

type instance V (BarPlot n) = V2
type instance N (BarPlot n) = n

instance OrderedField n => Enveloped (BarPlot n) where
  getEnvelope BarPlot {..} =
    orient bOrient _reflectXY id . getEnvelope . (id :: Path v n -> Path v n) $
      foldMap drawBar bData

instance (Typeable b, TypeableFloat n, Renderable (Path V2 n) b)
    => Plotable (BarPlot n) b where
  renderPlotable s BarPlot {..} pp =
    foldMap drawBar bData
      # applyAreaStyle pp
      # transform (view specTrans s <> orient bOrient _reflectionXY mempty)

  defLegendPic BarPlot {..} pp
    = centerXY
    . applyAreaStyle pp'
    . orient bOrient _reflectXY id
    $ d
    where
      -- Multiple bars get two bars next to each other for the legend. A
      -- single bar only gets one bar.
      d | has (ix 1) bData = alignB (rect 4 7) ||| strutX 3 ||| alignB (rect 4 10)
        | otherwise        = rect 4 10

      -- The legend bars don't look right if the line width is too big so we limit it
      pp' = pp & areaStyle . mapped . _lw %~ atMost (local 0.8)

-- Options -------------------------------------------------------------

-- | Options for simple bar plots. The following lenses can be used to
-- adjust 'BarOptions':
--
--   * @'orientation' : 'Orientation'@ - direction of bars
--   * @'barWidth'    : n@ - width of each bar
--   * @'barSpacing'  : n@ - spacing of bars
--   * @'barStart'    : n@ - starting position for bars
data BarOptions n = BarOptions
  { barOptsOrientation :: Orientation
  , barOptsWidth       :: n
  , barOptsSpacing     :: n
  , barOptsStart       :: n
  , barOptsIndividualWidth :: Rational
    -- Extra options for grouped bars?
  }

type instance N (BarOptions n) = n

instance Fractional n => Default (BarOptions n) where
  def = BarOptions
    { barOptsOrientation = Vertical
    , barOptsWidth       = 0.8
    , barOptsSpacing     = 1
    , barOptsStart       = 1
    , barOptsIndividualWidth = 1
    }

instance HasOrientation (BarOptions n) where
  orientation = lens barOptsOrientation (\opts o -> opts {barOptsOrientation = o})

-- | Class of things that can change the 'BarOptions'
class HasOrientation a => HasBarOptions a where
  -- | Options for how the bars of a bar plot should be drawn.
  barOptions :: Lens' a (BarOptions (N a))

instance HasBarOptions (BarOptions n) where
  barOptions = id

-- | The width bar for single / stacked bars or the width of a group
--   for grouped bar plot.
--
--   Default is @0.8@
barWidth :: HasBarOptions a => Lens' a (N a)
barWidth = barOptions . lens barOptsWidth (\bo w -> bo {barOptsWidth = w})

-- | The spacing between each bar or group of bars.
--
--   Default is @1@
barSpacing :: HasBarOptions a => Lens' a (N a)
barSpacing = barOptions . lens barOptsSpacing (\bo w -> bo {barOptsSpacing = w})

-- | The distance from the axis to centre of the first bar.
--
--   Default is @1@
barStart :: HasBarOptions a => Lens' a (N a)
barStart = barOptions . lens barOptsStart (\bo x -> bo {barOptsStart = x})

-- | Width multiplier for individual bars in group bar plots.
barIndividualWidth :: HasBarOptions a => Lens' a Rational
barIndividualWidth = barOptions
                   . lens barOptsIndividualWidth
                          (\bo x -> bo {barOptsIndividualWidth = x})

-- Helper functions ----------------------------------------------------

-- | Create equidistant bars using the values.
mkUniformBars
  :: Num n
  => BarOptions n
  -> [(n,n)] -- ^ values
  -> BarPlot n
mkUniformBars BarOptions {..} ys
  = BarPlot (imap mkBar ys) barOptsOrientation
  where mkBar i y = ABar (barOptsStart + fromIntegral i * barOptsSpacing) y barOptsWidth

-- | Create uniform bars from groups of data, placing one group after
--   the other.
mkMultiRunning
  :: Num n
  => BarOptions n
  -> [[(n,n)]] -- values
  -> [BarPlot n]
mkMultiRunning bo = snd . foldr f (barOptsStart bo, [])
  where
    f d (x, bs) = (x + dx, mkUniformBars bo {barOptsStart = x} d : bs)
      where dx = barOptsSpacing bo * fromIntegral (length d)

-- | Create uniform bars from groups of data, placing one on top of the
--   other. The first list will be the same as @mkUniformBars opts (map
--   (0,) ys)@, subsequent lists will be placed on top.
mkMultiStacked
  :: Num n
  => BarOptions n
  -> [[n]] -- values
  -> [BarPlot n]
mkMultiStacked bo = snd . List.mapAccumR f (repeat 0)
  where
    -- y0s are the base values for this set of bars, these accumulate
    -- for each set of data
    f y0s ys = (y1s, mkUniformBars bo ds)
      where y1s  = liftU2 (+) y0s ys
            ds   = zipWith (\y0 y -> (y0, y0 + y)) y0s ys

-- | Similar to 'mkMultiStacked' but stack has the same height.
mkMultiStackedEqual
  :: Fractional n
  => n     -- ^ value each bar reaches
  -> BarOptions n
  -> [[n]] -- ^ values
  -> [BarPlot n]
mkMultiStackedEqual yM bo yss = mkMultiStacked bo yss'
  where
    -- Multiplier for each bar to reach the desired height.
    ms = map (\ys -> yM / sum ys) $ List.transpose yss

    -- Normalise each data set by multiplying it with the normalising
    -- factor.
    yss' = map (zipWith (*) ms) yss

-- | Make bars that are grouped together. Each group of bars is treated
--   as a single bar when using the 'BarPlotsOpts'. There is an addition
--   parameter to adjust the width of each individual bar.
mkGrouped
  :: Fractional n
  => BarOptions n
  -> [[n]]
  -> [BarPlot n]
mkGrouped bo xs =
  flip imap xs $ \i ns ->
    mkUniformBars
      bo { barOptsStart = start' + width' * fromIntegral i
         , barOptsWidth = width' * fromRational (barOptsIndividualWidth bo)
         }
      (map (0,) ns)
  where
    n = fromIntegral $ length xs
    -- start' is such that middle of the middle bar is now at
    -- barOptsStart bo
    start' = barOptsStart bo - (n - 1) * width' / 2
    width' = barOptsWidth bo / n

-- temporary functions that will be in next lib release

_reflectionXY :: (Additive v, R2 v, Num n) => Transformation v n
_reflectionXY = fromSymmetric $ (_xy %~ view _yx) <-> (_xy %~ view _yx)

_reflectXY :: (InSpace v n t, R2 v, Transformable t) => t -> t
_reflectXY = transform _reflectionXY


----------------------------------------------------------------------------------
-- State API
----------------------------------------------------------------------------------

-- This is an experimental api to help with typical bar charts. The
-- implementation is quite complicated and probably over engineered. In
-- the future more options like values on top bars / error bars etc
-- could be added to this setup.

-- Bar state -----------------------------------------------------------

data MultiBarState b n a = MultiBarState
  { mbsOptions :: BarOptions n
      -- options used to draw the bars
  , mbsData    :: [(a, Endo (PlotProperties b V2 n))]
      -- the data along with an adjustment to the plot properties
  , mbsLabels  :: [String]
      -- labels to be placed at the bottom of each bar
  }

type instance N (MultiBarState b n a) = n

instance HasOrientation (MultiBarState b n a) where
  orientation = barOptions . orientation

instance HasBarOptions (MultiBarState b n a) where
  barOptions = lens mbsOptions (\mbs o -> mbs {mbsOptions = o})

class HasLabels a where
  labels :: Lens' a [String]

instance HasLabels (MultiBarState b n a) where
  labels = lens mbsLabels (\mbs ls -> mbs {mbsLabels = ls})

-- | Labels to use for each bar (or group of bars) along the axis.
labelBars :: HasLabels a => [String] -> State a ()
labelBars xs = labels .= xs

-- | Given the data for the bar, modify the properties for the bar that
--   uses that data.
modifyBars :: (a -> State (PlotProperties b V2 n) ()) -> State (MultiBarState b n a) ()
modifyBars f =
  _mbsData . mapped %= \(a, endo) -> (a, endo <> Endo (execState (f a)))
    where
      _mbsData = lens mbsData (\bs d -> bs {mbsData = d})

-- Compiling bar charts ------------------------------------------------

data BarState b n = BarState
  { bsOptions :: BarOptions n
  , bsEndo    :: Endo (PlotProperties b V2 n)
  , bsLabels  :: [String]
  }

type instance N (BarState b n) = n

instance HasOrientation (BarState b n) where
  orientation = barOptions . orientation

instance HasBarOptions (BarState b n) where
  barOptions = lens bsOptions (\bs o -> bs {bsOptions = o})

instance HasLabels (BarState b n) where
  labels = lens bsLabels (\bs ls -> bs {bsLabels = ls})

-- | Bar plot for bars starting at 0.
bars
  :: (MonadState (Axis b V2 n) m,
      Renderable (Path V2 n) b,
      Typeable b,
      Foldable f,
      TypeableFloat n)
  => f n
  -> m ()
bars ns = bars' ns (return ())

-- | Bar plot for bars starting at 0 with the ability to change the bar
--   options.
bars'
  :: (MonadState (Axis b V2 n) m,
      Renderable (Path V2 n) b,
      Typeable b,
      Foldable f,
      TypeableFloat n)
  => f n
  -> State (BarState b n) ()
  -> m ()
bars' (F.toList -> ns) = barsRanged' (map (0,) ns)

-- | Bar plot for ranged bars with the ability to change the bar
--   options.
barsRanged'
  :: (MonadState (Axis b V2 n) m,
      Renderable (Path V2 n) b,
      Typeable b,
      Foldable f,
      TypeableFloat n)
  => f (n,n)
  -> State (BarState b n) ()
  -> m ()
barsRanged' (F.toList -> ns) st = do
  addPlotable' b $ plotProperties %= appEndo (bsEndo bs)
  barAxisLabels (bs ^. barOptions) (bsLabels bs)
    where
      b = mkUniformBars (bs ^. barOptions) ns
      bs  = execState st bs0
      bs0 = BarState def mempty []

-- | Generic function used to build other multi bar functions.
barMultiHelper
  :: (MonadState (Axis b V2 n) m,
      Renderable (Path V2 n) b,
      Typeable b,
      Foldable f,
      Foldable g,
      TypeableFloat n)
  => (BarOptions n -> [[n]] -> [BarPlot n]) -- ^ low level bar plot function
  -> f a -- ^ foldable container over custom data @a@
  -> (a -> g n) -- ^ function to extract data from @a@
  -> State (MultiBarState b n a) () -- ^ MultiBarState modifier
  -> m ()
barMultiHelper barF (F.toList -> as0) f st = do
  F.forM_ propertiedBars $ \(b,endo) ->
    addPlotable' b $ plotProperties %= appEndo endo

  barAxisLabels (bs ^. barOptions) (bs ^. labels)
  where

    -- bars
    propertiedBars = zip barPlots endos
    barPlots = barF (bs ^. barOptions) $ map (F.toList . f) as

    -- data and modifiers
    (as, endos) = unzip $ mbsData bs

    -- bar state
    bs  = execState st bs0
    bs0 = MultiBarState
      { mbsOptions = def
      , mbsData    = map (\a -> (a, mempty)) as0
      , mbsLabels  = []
      }

-- | Place labels under the centre of each bar using the 'BarOptions'.
barAxisLabels
  :: (MonadState (Axis b V2 n) m, Fractional n)
  => BarOptions n -> [String] -> m ()
barAxisLabels bo ls =
  unless (null ls) $ do
    axisTicks . orient bo _y _x . majorTicksFun . mapped .= xs
    axisTickLabels . orient bo _y _x . tickLabelFun .= \_ _ -> zip xs ls
  where
    xs = map ((+ view barStart bo) . (* view barSpacing bo) . fromIntegral)
             [0 .. length ls - 1]

-- Grouped -------------------------------------------------------------

-- | Make a grouped bar plot from groups of lists of values.
barGrouped
  :: (MonadState (Axis b V2 n) m,
      Renderable (Path V2 n) b,
      Typeable b,
      Foldable f,
      Foldable g,
      TypeableFloat n)
  => f (g n)
  -> m ()
barGrouped nss = barGrouped' nss id (return ())

-- | Make a grouped bar plot from a set of data, a way to extract the
--   values from the data and a state modifier for the bars.
barGrouped'
  :: (MonadState (Axis b V2 n) m,
      Renderable (Path V2 n) b,
      Typeable b,
      Foldable f,
      Foldable g,
      TypeableFloat n)
  => f a
  -> (a -> g n)
  -> State (MultiBarState b n a) ()
  -> m ()
barGrouped' = barMultiHelper mkGrouped

-- Running -------------------------------------------------------------

-- | Make a grouped bar plot from groups of lists of values.
barRunning
  :: (MonadState (Axis b V2 n) m,
      Renderable (Path V2 n) b,
      Typeable b,
      Foldable f,
      Foldable g,
      TypeableFloat n)
  => f (g n)
  -> m ()
barRunning nss = barRunning' nss id (return ())

-- | Make a grouped bar plot from a set of data, a way to extract the
--   values from the data and a state modifier for the bars.
barRunning'
  :: (MonadState (Axis b V2 n) m,
      Renderable (Path V2 n) b,
      Typeable b,
      Foldable f,
      Foldable g,
      TypeableFloat n)
  => f a
  -> (a -> g n)
  -> State (MultiBarState b n a) ()
  -> m ()
barRunning' = barMultiHelper (\bo nss -> mkMultiRunning bo (map (map (0,)) nss))

-- Stacked -------------------------------------------------------------

-- | Make a grouped bar plot from groups of lists of values.
barStacked
  :: (MonadState (Axis b V2 n) m,
      Renderable (Path V2 n) b,
      Typeable b,
      Foldable f,
      Foldable g,
      TypeableFloat n)
  => f (g n)
  -> m ()
barStacked nss = barStacked' nss id (return ())

-- | Make a grouped bar plot from a set of data, a way to extract the
--   values from the data and a state modifier for the bars.
barStacked'
  :: (MonadState (Axis b V2 n) m,
      Renderable (Path V2 n) b,
      Typeable b,
      Foldable f,
      Foldable g,
      TypeableFloat n)
  => f a
  -> (a -> g n)
  -> State (MultiBarState b n a) ()
  -> m ()
barStacked' = barMultiHelper mkMultiStacked

-- | Make a grouped bar plot from groups of lists of values.
barStackedEqual
  :: (MonadState (Axis b V2 n) m,
      Renderable (Path V2 n) b,
      Typeable b,
      Foldable f,
      Foldable g,
      TypeableFloat n)
  => n
  -> f (g n)
  -> m ()
barStackedEqual l nss = barStackedEqual' l nss id (return ())

-- | Make a grouped bar plot from a set of data, a way to extract the
--   values from the data and a state modifier for the bars.
barStackedEqual'
  :: (MonadState (Axis b V2 n) m,
      Renderable (Path V2 n) b,
      Typeable b,
      Foldable f,
      Foldable g,
      TypeableFloat n)
  => n
  -> f a
  -> (a -> g n)
  -> State (MultiBarState b n a) ()
  -> m ()
barStackedEqual' l = barMultiHelper (mkMultiStackedEqual l)

