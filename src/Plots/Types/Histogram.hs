{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TypeFamilies              #-}

module Plots.Types.Histogram
  (

-- * GHistogramPlot plot
     GHistogramPlot
  , _HistogramPlot

  , HistogramPlot
  , mkHistogramPlotOf
  , mkHistogramPlot

  , createBarData'
  , setBin

  ) where

import           Control.Lens                    hiding (lmap, none, transform,
                                                  ( # ))
import qualified Data.Foldable                   as F
import           Data.Typeable
import           Data.List
import           Data.Function

import           Diagrams.Prelude

import           Diagrams.Coordinates.Isomorphic

import           Plots.Themes
import           Plots.Types

data GHistogramPlot v n a = forall s. GHistogramPlot
  { hData :: s
  , hFold :: Fold s a
  , hPos  :: a -> Point v n
  , hFunc :: Int -> [P2 n] -> [P2 n]
-- change P2 n to Point v n
-- need to add v ~ V2 every where, both in mkHistogramPlot and BinY1
-- also change in some places in Plots.hs
  , hBin  :: Int 
  } deriving Typeable

type instance V (GHistogramPlot v n a) = v
type instance N (GHistogramPlot v n a) = n

instance (Metric v, OrderedField n) => Enveloped (GHistogramPlot v n a) where
  getEnvelope GHistogramPlot {..} = foldMapOf (hFold . to hPos) getEnvelope hData

instance (Typeable a, Typeable b, TypeableFloat n, Renderable (Path V2 n) b)
    => Plotable (GHistogramPlot V2 n a) b where
  renderPlotable s GHistogramPlot {..} pp =
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
                         # applyBarStyle pp
                         # transform t

  defLegendPic GHistogramPlot {..} pp
      = square 5 # applyBarStyle pp

------------------------------------------------------------------------
-- Simple Histogram Plot
------------------------------------------------------------------------

type HistogramPlot v n = GHistogramPlot v n (Point v n)

mkHistogramPlot :: (PointLike v n p, F.Foldable f, Ord n, Fractional n, Enum n, Num n)
              => f p -> HistogramPlot v n
mkHistogramPlot = mkHistogramPlotOf folded


mkHistogramPlotOf :: (PointLike v n p, Ord n, Fractional n, Enum n, Num n)
                => Fold s p -> s -> HistogramPlot v n
mkHistogramPlotOf f a = GHistogramPlot
  { hData = a
  , hFold = f . unpointLike
  , hPos  = id
  , hFunc = binY
  , hBin  = 10 
  }
  
createBarData' z w = map p2 [(xmax, y),(xmin, y),(xmin, 0),(xmax, 0)]
        where xmax =  x + (w/2)
              xmin =  x - (w/2)
              (x, y) = unp2 z

_HistogramPlot :: (Plotable (HistogramPlot v n) b, Typeable b)
                   => Prism' (Plot b v n) (HistogramPlot v n)
_HistogramPlot = _Plot

---------- add more of this function - one for mean other for sum --

binY :: (Ord n, Fractional n, Enum n) => Int -> [P2 n] -> [P2 n]
binY b xs =  map p2 (zip xpts ypts)
              where xmin = fst (maximumBy (compare `on` fst) (map unp2 xs))
                    xmax = fst (minimumBy (compare `on` fst) (map unp2 xs))
                    xpts = [xmin, (xmin + w) .. xmax]
                    ypts = [bin1D xs (xpt, (xpt + w)) | xpt <- xpts]
                    w    = (xmax - xmin)/ fromIntegral b

bin1D xs (a,b) = mean [y | (x,y) <- (map unp2 xs), x > b, x < a]

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

instance HasHistogram (PropertiedPlot (GHistogramPlot v n d) b) v n d where
  histogram = _pp

