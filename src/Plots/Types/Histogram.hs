{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Plots.Types.Histogram
-- Copyright   :  (C) 2015 Christopher Chalmers
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Christopher Chalmers
-- Stability   :  experimental
-- Portability :  non-portable

-- A histogram is a graphical representation of the distribution of
-- numerical data. It is an estimate of the probability distribution of
-- a continuous variable.
--
----------------------------------------------------------------------------

module Plots.Types.Histogram
  (
    -- * Histogram plot
    HistogramPlot

    -- ** Already computed histograms
  , computedHistogram

    -- ** Histogram options
  , HistogramOptions
  , HasHistogramOptions (..)

    -- ** Normalisation
  , NormalisationMethod
  , count
  , probability
  , countDensity
  , pdf
  , cumilative
  , cdf

    -- ** Plotting histograms
  , histogramPlot
  , histogramPlot'
  , histogramPlotOf
  , histogramPlotOf'

    -- * Low level constructors
  , mkComputedHistogram
  , mkHistogramPlot
  ) where

import           Control.Monad.State.Lazy

import qualified Data.Foldable               as F
import           Data.Function
import           Data.Maybe
import           Data.Typeable

import qualified Data.Vector                 as V
import qualified Statistics.Sample.Histogram as Stat

import           Diagrams.Core.Transform     (fromSymmetric)
import           Diagrams.Prelude
import           Linear.V2                   (_yx)

import           Plots.Axis
import           Plots.Style
import           Plots.Types
import           Plots.Utils


-- | Construct a rectangle of size v with the bottom left at point p.
rectBL :: (InSpace V2 n t, TrailLike t) => Point V2 n -> V2 n -> t
rectBL p (V2 x y) =
  trailLike $ fromOffsets [V2 x 0, V2 0 y, V2 (-x) 0] # closeTrail `at` p

------------------------------------------------------------------------
-- GHistogram plot
------------------------------------------------------------------------

-- | Simple histogram type supporting uniform bins.
data HistogramPlot n = HistogramPlot
  { hWidth  :: n
  , hStart  :: n
  , hValues :: [n]
  , hOrient :: Orientation
  }

type instance V (HistogramPlot n) = V2
type instance N (HistogramPlot n) = n

instance OrderedField n => Enveloped (HistogramPlot n) where
  getEnvelope HistogramPlot {..} =
    -- don't like this reduntent code
    getEnvelope . orient hOrient _reflectXY id . (id :: Path v n -> Path v n) $
      ifoldMap drawBar hValues
    where
      drawBar i h = rectBL (mkP2 x 0) (V2 hWidth h)
        where x = hStart + fromIntegral i * hWidth

instance (Typeable b, TypeableFloat n, Renderable (Path V2 n) b)
    => Plotable (HistogramPlot n) b where
  renderPlotable s sty HistogramPlot {..} =
    ifoldMap drawBar hValues
      # orient hOrient _reflectXY id
      # applyAreaStyle sty
      # transform (s^.specTrans)
    where
      drawBar i h = rectBL (mkP2 x 0) (V2 hWidth h)
        where x = hStart + fromIntegral i * hWidth

  defLegendPic sty HistogramPlot {..}
    = centerXY
    . applyAreaStyle sty'
    . orient hOrient _reflectXY id
    $ alignB (rect 4 7) ||| alignB (rect 4 10) ||| alignB (rect 4 6)
    where
      -- The legend bars don't look right if the line width is too big so we limit it
      sty' = sty & areaStyle . _lw %~ atMost (local 0.8)

instance HasOrientation (HistogramPlot n) where
  orientation = lens hOrient $ \hp o -> hp {hOrient = o}

------------------------------------------------------------------------
-- Simple histogram plot
------------------------------------------------------------------------

-- | Plot an already computed histogram with equally sized bins.
computedHistogram
  :: (MonadState (Axis b V2 n) m,
      Plotable (HistogramPlot n) b,
      Foldable f, Typeable b, TypeableFloat n)
  => n   -- ^ start of first bin
  -> n   -- ^ width of each bin
  -> f n -- ^ heights of the bins
  -> State (Plot (HistogramPlot n) b) ()
  -> m ()
computedHistogram x0 w xs = addPlotable (mkComputedHistogram x0 w xs)

-- | Construct a 'HistogramPlot' from raw histogram data.
mkComputedHistogram
  :: (Foldable f, OrderedField n)
  => n -- ^ start of first bin
  -> n -- ^ width of each bin
  -> f n -- ^ heights of the bins
  -> HistogramPlot n
mkComputedHistogram x0 w xs = HistogramPlot x0 w (F.toList xs) Horizontal

----------------------------------------------------------------------------
-- Building histograms
----------------------------------------------------------------------------

-- Histogram options ---------------------------------------------------

-- | The way to normalise the data from a histogram. The default method
--   is 'count'.
newtype NormalisationMethod =
  NM { runNM :: forall n. Fractional n => n -> V.Vector n -> V.Vector n }
 -- width -> heights -> normalised heights

instance Default NormalisationMethod where
  def = count

-- | The height of each bar is the number of observations. This is the
--   'Default' method.
count :: NormalisationMethod
count = NM $ \_ v -> v

-- | The sum of the heights of the bars is equal to 1.
probability :: NormalisationMethod
probability = NM $ \_ v -> v ^/ V.sum v

-- | The height of each bar is @n / w@ where @n@ is the number of
--   observations and @w@ is the width.
countDensity :: NormalisationMethod
countDensity = NM $ \w v -> v ^/ w

-- | The total area of the bars is @1@. This gives a probability density
--   function estimate.
pdf :: NormalisationMethod
pdf = NM $ \w v -> v ^/ (w * V.sum v)

-- | The height of each bar is the cumulative number of observations in
--   each bin and all previous bins. The height of the last bar is the
--   total number of observations.
cumilative :: NormalisationMethod
cumilative = NM $ \_ -> V.scanl1 (+)

-- | Cumulative density function estimate. The height of each bar is
--   equal to the cumulative relative number of observations in the bin
--   and all previous bins. The height of the last bar is 1.
cdf :: NormalisationMethod
cdf = NM $ \_ v -> V.scanl1 (+) v ^/ V.sum v

-- | Options for binning histogram data. For now only very basic
--   histograms building is supported.
data HistogramOptions n = HistogramOptions
  { hBins   :: Int
  , hRange  :: Maybe (n, n)
  , hNorm   :: NormalisationMethod
  , oOrient :: Orientation
  }

type instance V (HistogramOptions n) = V2
type instance N (HistogramOptions n) = n

instance Default (HistogramOptions n) where
  def = HistogramOptions
    { hBins   = 10
    , hRange  = Nothing
    , hNorm   = def
    , oOrient = Vertical
    }

instance HasOrientation (HistogramOptions n) where
  orientation = lens oOrient $ \ho o -> ho {oOrient = o}

class HasOrientation a => HasHistogramOptions a where
  -- | Options for building the histogram from data.
  histogramOptions :: Lens' a (HistogramOptions (N a))

  -- | The number of bins (bars) to use for the histogram. Must be
  --   positive.
  --
  --   'Default' is @10@.
  numBins :: Lens' a Int
  numBins = histogramOptions . lens hBins (\ho n -> ho {hBins = n})

  -- | The range of data to consider when building the histogram. Any
  --   data outside the range is ignored.
  --
  --   'Default' is 'Nothing'.
  binRange :: Lens' a (Maybe (N a, N a))
  binRange = histogramOptions . lens hRange (\ho r -> ho {hRange = r})

  -- | Should the resulting histogram be normalised so the total area is
  --   1.
  --
  --   'Default' is False.
  normaliseSample :: Lens' a NormalisationMethod
  normaliseSample = histogramOptions . lens hNorm (\ho b -> ho {hNorm = b})

instance HasHistogramOptions (HistogramOptions n) where
  histogramOptions = id

instance HasHistogramOptions a => HasHistogramOptions (Plot a b) where
  histogramOptions = rawPlot . histogramOptions

-- | Create a histogram by binning the data using the
--   'HistogramOptions'.
mkHistogramPlot
  :: (Foldable f, RealFrac n)
  => HistogramOptions n -> f n -> HistogramPlot n
mkHistogramPlot HistogramOptions {..} xs =
  HistogramPlot
    { hWidth  = w
    , hStart  = a
    , hValues = V.toList $ runNM hNorm w ns
    , hOrient = Vertical
    }
  where
    w     = (b - a) / fromIntegral hBins
    ns    = Stat.histogram_ hBins a b v
    v     = V.fromList (F.toList xs)
    (a,b) = fromMaybe (range hBins v) hRange

-- Taken from Statistics, which was limited to 'Double'.
range :: (Ord n, Fractional n)
      => Int                    -- ^ Number of bins (must be positive).
      -> V.Vector n             -- ^ Sample data (cannot be empty).
      -> (n, n)
range nBins xs
    | nBins < 1 = error "Plots.Types.Histogram: invalid bin count"
    | V.null xs = error "Plots.Types.Histogram: empty sample"
    | lo == hi  = case abs lo / 10 of
                    a | a < 1e-6   -> (-1,1)
                      | otherwise  -> (lo - a, lo + a)
    | otherwise = (lo-d, hi+d)
  where
    d | nBins == 1 = 0
      | otherwise  = (hi - lo) / ((fromIntegral nBins - 1) * 2)
    (lo,hi)        = minMaxOf folded xs
{-# INLINE range #-}

-- |
-- mkWeightedHistogram
--   :: (Foldable f, OrderdField n)
--   => HistogramOptions n -> [(n, n)] -> HistogramPlot n
-- mkWeightedHistogram

------------------------------------------------------------------------
-- Histogram
------------------------------------------------------------------------

-- $ histogram
-- Histograms display data as barplot of x data, bin y data.
-- Box plots have the following lenses:
--
-- @
-- * 'setBin' :: 'Lens'' ('BoxPlot' v n) 'Double' - 10
-- @

-- | Add a 'HistogramPlot' to the 'AxisState' from a data set.
--
-- @
--   myaxis = r2Axis ~&
--     histogramPlot data1
-- @
--
-- === __Example__
--
-- <<plots/histogram.png#diagram=histogram&width=300>>
--
-- @
-- fillOpacity = barStyle . mapped . _opacity
--
-- myaxis :: Axis B V2 Double
-- myaxis = r2Axis &~ do
--  histogramPlot' mydata1 $ do
--     addLegendEntry "histogram"
--     plotColor .= blue
--     fillOpacity .= 0.5
-- @

histogramPlot
  :: (MonadState (Axis b V2 n) m, Plotable (HistogramPlot n) b, F.Foldable f, RealFrac n)
  => f n
  -> State (Plot (HistogramOptions n) b) ()
  -> m ()
histogramPlot ns s = addPlot (hoPlot & rawPlot %~ \ho -> mkHistogramPlot ho ns)
  where hoPlot = mkPlot def &~ s

-- | Make a 'HistogramPlot' and take a 'State' on the plot to alter it's
--   options
--
-- @
--   myaxis = r2Axis &~ do
--     histogramPlot' pointData1 $ do
--       setBin .= 30
--       addLegendEntry "data 1"
-- @

histogramPlot'
  :: (MonadState (Axis b V2 n) m, Plotable (HistogramPlot n) b, F.Foldable f, RealFrac n)
  => f n
  -> m ()
histogramPlot' d = histogramPlot d (return ())

-- | Add a 'HistogramPlot' using a fold over the data.
histogramPlotOf
  :: (MonadState (Axis b V2 n) m, Plotable (HistogramPlot n) b, RealFrac n)
  => Fold s n -- ^ fold over the data
  -> s        -- ^ data to fold
  -> State (Plot (HistogramOptions n) b) () -- ^ change to the plot
  -> m () -- ^ add plot to the 'Axis'
histogramPlotOf f s = histogramPlot (toListOf f s)

-- | Same as 'histogramPlotOf' without any changes to the plot.
histogramPlotOf'
  :: (MonadState (Axis b V2 n) m, Plotable (HistogramPlot n) b, RealFrac n)
  => Fold s n -> s -> m ()
histogramPlotOf' f s = histogramPlotOf f s (return ())

-- temporary functions that will be in next lib release

_reflectionXY :: (Additive v, R2 v, Num n) => Transformation v n
_reflectionXY = fromSymmetric $ (_xy %~ view _yx) <-> (_xy %~ view _yx)

_reflectXY :: (InSpace v n t, R2 v, Transformable t) => t -> t
_reflectXY = transform _reflectionXY

