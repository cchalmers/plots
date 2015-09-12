{-# LANGUAGE DeriveDataTypeable    #-}
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

    -- ** Options

  , BarOptions
  , barWidth
  , barSpacing
  , barStart

    -- Low level constructors
  , mkUniformBars
  , mkMultiAdjacent
  , mkMultiStacked
  , mkMultiStackedEqual
  , mkGrouped

  -- * Internal bar type
  , ABar (..)

  ) where

import           Control.Lens            hiding (at, none, transform, ( # ))
import           Data.Typeable

import           Plots.Themes
import           Plots.Types

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

-- | A bar plot for a single set of bars. Multi-bar plots are acheived
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
      # applyBarStyle pp
      # transform (view specTrans s <> orient bOrient _reflectionXY mempty)

  defLegendPic BarPlot {..} pp
    = centerXY
    . applyBarStyle pp'
    . orient bOrient _reflectXY id
    $ d
    where
      -- Multiple bars get two bars next to each other for the legend. A
      -- single bar only gets one bar.
      d | has (ix 1) bData = alignB (rect 4 7) ||| strutX 3 ||| alignB (rect 4 10)
        | otherwise        = rect 4 10

      -- The legend bars don't look right if the line width is too big so we limit it
      pp' = pp & barStyle . mapped . _lw %~ atMost (local 0.8)

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
    -- Extra options for grouped bars?
  }

type instance N (BarOptions n) = n

instance Fractional n => Default (BarOptions n) where
  def = BarOptions
    { barOptsOrientation = Vertical
    , barOptsWidth       = 0.8
    , barOptsSpacing     = 1
    , barOptsStart       = 1
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
mkMultiAdjacent
  :: Num n
  => BarOptions n
  -> [[(n,n)]] -- values
  -> [BarPlot n]
mkMultiAdjacent bo = snd . foldr f (barOptsStart bo, [])
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
  => n -- ^ multiplier for each single bar width, so 1 the bars in a group are touching.
  -> BarOptions n
  -> [[n]]
  -> [BarPlot n]
mkGrouped m bo xs =
  flip imap xs $ \i ns ->
    mkUniformBars
      bo { barOptsStart = start' + width' * fromIntegral i
         , barOptsWidth = width' * m
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

