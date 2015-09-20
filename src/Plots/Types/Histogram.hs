{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE StandaloneDeriving        #-}

module Plots.Types.Histogram
  (  -- * GHistogramPlot plot
     GHistogramPlot
  -- , _HistogramPlot

    -- * Histogram plot
  , HistogramPlot
  , mkHistogramPlotOf
  , mkHistogramPlot

    -- * Helper functions
  , createBarData'

    -- * Histogram lenses
  , setBin

    -- * Histogram
  , histogramPlot
  , histogramPlot'
  -- , histogramPlotL

    -- * Fold variant histogram
  , histogramPlotOf
  , histogramPlotOf'
  -- , histogramPlotLOf
  ) where

import           Control.Lens                    hiding (lmap, none, transform,
                                                  ( # ))
import           Control.Monad.State.Lazy

import qualified Data.Foldable                   as F
import           Data.Typeable
import           Data.List
import           Data.Function

import           Diagrams.Prelude
import           Diagrams.Coordinates.Isomorphic

import           Plots.Style
import           Plots.Types
import           Plots.Axis

------------------------------------------------------------------------
-- GHistogram plot
------------------------------------------------------------------------

data GHistogramPlot v n a = forall s. GHistogramPlot
  { hData :: s
  , hFold :: Fold s a
  , hPos  :: a -> Point v n
  , hFunc :: Int -> [P2 n] -> [P2 n]
  , hBin  :: Int
  } deriving Typeable

-- change P2 n to Point v n.
-- need to add v ~ V2, both in mkHistogramPlot and BinY1.
-- also change the same in api.

type instance V (GHistogramPlot v n a) = v
type instance N (GHistogramPlot v n a) = n

instance (Metric v, OrderedField n) => Enveloped (GHistogramPlot v n a) where
  getEnvelope GHistogramPlot {..} = foldMapOf (hFold . to hPos) getEnvelope hData

instance (Typeable a, Typeable b, TypeableFloat n, Renderable (Path V2 n) b)
    => Plotable (GHistogramPlot V2 n a) b where
  renderPlotable s _opts sty GHistogramPlot {..} =
      mconcat [drawbar (createBarData' z w) | z <- zs ]
    where
      ps = toListOf (hFold . to hPos . to (logPoint ls)) hData
      w  = ( xmax - xmin )/ fromIntegral hBin
      zs = hFunc hBin ps
      t  = s ^. specTrans
      ls = s ^. specScale
      xmin = fst (head (sortBy (compare `on` fst) (map unp2 ps)))
      xmax = fst (last (sortBy (compare `on` fst) (map unp2 ps)))
      drawbar barpts = fromVertices barpts
                         # mapLoc closeLine
                         # stroke
                         # applyAreaStyle sty
                         # transform t

  defLegendPic GHistogramPlot {..} sty
      = square 5 # applyAreaStyle sty

-- _HistogramPlot :: (Plotable (HistogramPlot v n) b, Typeable b)
--                    => Prism' (Plot b v n) (HistogramPlot v n)
-- _HistogramPlot = _Plot

------------------------------------------------------------------------
-- Simple histogram plot
------------------------------------------------------------------------

type HistogramPlot v n = GHistogramPlot v n (Point v n)

-- | Plot a histogram by averaging x data with y bin = 0.
mkHistogramPlot :: (PointLike v n p, F.Foldable f, Ord n, Fractional n, Enum n, Num n)
              => f p -> HistogramPlot v n
mkHistogramPlot = mkHistogramPlotOf folded

-- | Plot a histogram using a given fold.
mkHistogramPlotOf :: (PointLike v n p, Ord n, Fractional n, Enum n, Num n)
                => Fold s p -> s -> HistogramPlot v n
mkHistogramPlotOf f a = GHistogramPlot
  { hData = a
  , hFold = f . unpointLike
  , hPos  = id
  , hFunc = binY
  , hBin  = 10
  }

------------------------------------------------------------------------
-- Helper functions
------------------------------------------------------------------------

binY :: (Ord n, Fractional n, Enum n) => Int -> [P2 n] -> [P2 n]
binY b xs =  map p2 (zip xpts ypts)
              where xmin = fst (maximumBy (compare `on` fst) (map unp2 xs))
                    xmax = fst (minimumBy (compare `on` fst) (map unp2 xs))
                    xpts = [xmin, (xmin + w) .. xmax]
                    ypts = [bin1D xs (xpt, (xpt + w)) | xpt <- xpts]
                    w    = (xmax - xmin)/ fromIntegral b

bin1D :: (Fractional a, Ord a) => [P2 a] -> (a, a) -> a
bin1D xs (a,b) = mean [y | (x,y) <- (map unp2 xs), x > b, x < a]


createBarData' :: Fractional n => P2 n -> n -> [P2 n]
createBarData' z w = map p2 [(xmax, y),(xmin, y),(xmin, 0),(xmax, 0)]
        where xmax =  x + (w/2)
              xmin =  x - (w/2)
              (x, y) = unp2 z

mean :: (Num a, Fractional a) => [a] -> a
mean [] = 0.0
mean xs = (sum xs)/ fromIntegral (length xs)

----------------------------------------------------------------------------
-- Histogram Lenses
----------------------------------------------------------------------------

class HasHistogram a v n d | a -> v n, a -> d where
  histogram :: Lens' a (GHistogramPlot v n d)

  setBin :: Lens' a Int
  setBin = histogram . lens hBin (\hb bin -> hb {hBin = bin})

instance HasHistogram (GHistogramPlot v n d) v n d where
  histogram = id

instance HasHistogram (Plot (GHistogramPlot v n d) b) v n d where
  histogram = rawPlot

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
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (HistogramPlot v n) b,
      F.Foldable f, Enum n)
  => f p -> State (Plot (HistogramPlot v n) b) () -> m ()
histogramPlot d = addPlotable (mkHistogramPlot d)

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
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (HistogramPlot v n) b,
      F.Foldable f, Enum n)
  => f p -> m ()
histogramPlot' d = addPlotable' (mkHistogramPlot d)

-- | Add a 'HistogramPlot' with the given name for the legend entry.
--
-- @
--   myaxis = r2Axis &~ do
--     histogramPlotL "blue team" pointData1
--     histogramPlotL "red team" pointData2
-- @

-- histogramPlotL
--   :: (v ~ BaseSpace c,
--       PointLike v n p,
--       MonadState (Axis b c n) m,
--       Plotable (HistogramPlot v n) b,
--       F.Foldable f, Enum n)
--   => String -> f p -> m ()
-- histogramPlotL l d = addPlotableL l (mkHistogramPlot d)

-- Fold variants

histogramPlotOf
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (HistogramPlot v n) b, Enum n)
  => Fold s p -> s -> State (Plot (HistogramPlot v n) b) () -> m ()
histogramPlotOf f s = addPlotable (mkHistogramPlotOf f s)

histogramPlotOf'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (HistogramPlot v n) b, Enum n)
  => Fold s p -> s -> m ()
histogramPlotOf' f s = addPlotable' (mkHistogramPlotOf f s)

-- histogramPlotLOf
--   :: (v ~ BaseSpace c,
--       PointLike v n p,
--       MonadState (Axis b c n) m,
--       Plotable (HistogramPlot v n) b, Enum n)
--   => String -> Fold s p -> s -> m ()
-- histogramPlotLOf l f s = addPlotableL l (mkHistogramPlotOf f s)

