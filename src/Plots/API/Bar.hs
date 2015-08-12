{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE RecordWildCards       #-}

module Plots.Types.Bar
  ( GBarPlot (..)
  , createbardata
  , _BarPlot 

  , BarPlot (..)
  , mkBarPlot
--    BarPlot (..)
--  , simpleBarPlot
    -- * Prism
--  , _BarPlot

    -- * Lenses
--  , barWidth
--  , barSpacing
--  , verticleBars
--  , stacked
  ) where

import Control.Lens     hiding (transform, ( # ), none)

import Data.Maybe
import Data.Default
import Data.Typeable
import qualified Data.Foldable as F (Foldable, foldMap, toList)

import Diagrams.Prelude
import           Diagrams.Coordinates.Isomorphic

import Plots.Utils
import Plots.Themes
import Plots.Types


data GBarPlot = GBarPlot 
  { barData :: (Double, Double)
  , barWidth :: Double
  } deriving Typeable

type instance V GBarPlot = V2
--type instance N (GBarPlot n a) = n

makeLenses ''GBarPlot

instance Enveloped GBarPlot where
  getEnvelope GBarPlot {..} = getEnvelope (fromVertices (createbardata a b))
    where a = barData
          b = barWidth

createbardata (x, y) w = map p2 [(xmax, y),(xmin, y),(xmin, 0),(xmax, 0)]
        where xmax =  x + (w/2)
              xmin =  x - (w/2)


instance (Typeable b, TypeableFloat n, Renderable (Path V2 N) b)
    => Plotable GBarPlot b where
  renderPlotable s GBarPlot {..} pp =
      fromVertices ps
        # mapLoc closeLine
        # stroke
        # lw none
        # applyBarStyle pp
        # transform t

   <> fromVertices ps
        # mapLoc closeLine
        # stroke
        # transform t
        # applyLineStyle pp

    where
      ps = createbardata barData barWidth
      t  = s ^. specTrans
      ls = s ^. specScale

  defLegendPic GBarPlot {..} pp
      = square 5 # applyBarStyle pp

_BarPlot :: (Plotable GBarPlot b, Typeable b)
             => Prism' (Plot b v n) GBarPlot
_BarPlot = _Plot

------------------------------------------------------------------------
-- Bar Plot
------------------------------------------------------------------------

mkBarPlot :: (Num n)
                => (Double, Double) -> Double -> GBarPlot
mkBarPlot a w = GBarPlot
  { barData = a
  , barWidth = w
  }

{-
data BarPlot n = BarPlot
  { _barData       :: [(n,[n])] -- data for bars
  , _barWidth     :: n         -- total width of bars for one 'bit'
  , _barSpacing   :: n         -- gap between multibars in same value
  , _verticleBars :: Bool    -- whether the bars are verticle
  , _stacked      :: Bool    -- whether the bars stacked (or side by side)
  } deriving Typeable

type instance V (BarPlot n) = V2
type instance N (BarPlot n) = n

makeLenses ''BarPlot

instance OrderedField n => Enveloped (BarPlot n) where
  getEnvelope bp
    | nullOf barData bp = mempty
    | otherwise         = getEnvelope $ fromCorners (mkP2 xmin 0) (mkP2 xmax ymax)
    where
      ymax = fromMaybe 0 $ maximumOf (barData . each . _2 . each) bp
      V2 xmin xmax = minmaxOf (barData . each . _1) bp

instance (Typeable b, TypeableFloat n, Renderable (Path V2 n) b)
    => Plotable (BarPlot n) b where
  renderPlotable s d pp =
    drawBarPlot d
      # transform (s^.specTrans)
      # applyBarStyle pp

instance Fractional n => Default (BarPlot n) where
  def = BarPlot
          { _barData       = []
          , _barWidth     = 0.5
          , _barSpacing   = 0.1
          , _verticleBars = True
          , _stacked      = False
          }

-- TODO: work out a nice way to get different colours for multi-bar plots.

drawBarPlot :: (TypeableFloat n, Renderable (Path V2 n) b) => BarPlot n -> QDiagram b V2 n Any
drawBarPlot bp = F.foldMap makeBar (_barData bp)
  where
    tW = bp^.barWidth
    δ  = bp^.barSpacing
    --
    makeBar (_,[]) = mempty
    makeBar (x,bs) = ifoldMap mkBar bs
      where
        mkBar i h = rect w h
                      # alignB
                      # translateX (x + fromIntegral i * (δ + w))
        n = fromIntegral $ length bs
        w = recip n * (tW - δ * (n - 1))

-- instance (Typeable b, Renderable (Path R2) b) => Plotable (BarPlot b) b R2 where
--   plot _r _ t = transform t . drawBarPlot

simpleBarPlot :: (TypeableFloat n, F.Foldable f) => f n -> BarPlot n
simpleBarPlot (F.toList -> xs) = def { _barData = imap f xs }
  where
    f i h = (fromIntegral i + 1, [h])


_BarPlot :: Plotable (BarPlot n) b => Prism' (Plot b V2 n) (BarPlot n)
_BarPlot = _Plot
-}
------------------------------------------------------------------------
-- Histogram
------------------------------------------------------------------------


-- data Histogram n a = forall s. Histogram
--   { histogramData  :: s
--   , histogramFold  :: Fold s a
--   , histogramLowerLimit :: Maybe n
--   , histogramUpperLimit :: Maybe n
--   }
--
-- mkHistogramOf :: Fold s n -> s -> BarPlot n
-- mkHistogramOf f as =


