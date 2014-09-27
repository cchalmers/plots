{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Plots.Types.Bar
  ( BarPlot
  , simpleBarPlot
    -- * Prism
  , _BarPlot


    -- * Lenses
  , barPlotBars
  , barPlotWidth
  , barPlotSpacing
  , barPlotIsVerticle
  ) where

import Control.Lens     hiding (transform, ( # ))
import Data.Default
import Data.Typeable
import Data.Foldable (Foldable, foldMap, toList)
import Diagrams.Prelude

-- import Plots.Themes
import Plots.Types

data BarPlot b n = BarPlot
  { _barPlotBars     :: [(n,[n])] -- data for bars
  , _barPlotWidth    :: n         -- total width of bars for one 'bit'
  , _barPlotSpacing  :: n         -- gap between multibars in same value
  , _barPlotIsVerticle :: Bool    -- whether the bars are verticle
  , _barPlotGeneric  :: GenericPlot b V2 n
  } deriving Typeable

type instance V (BarPlot b n) = V2
type instance N (BarPlot b n) = n

makeLenses ''BarPlot

instance HasGenericPlot (BarPlot b n) b where
  genericPlot = barPlotGeneric

instance (TypeableFloat n, Renderable (Path V2 n) b) => Default (BarPlot b n) where
  def = BarPlot
          { _barPlotBars     = []
          , _barPlotWidth    = 0.5
          , _barPlotSpacing  = 0.1
          , _barPlotIsVerticle = True
          , _barPlotGeneric  = def
          }

-- TODO: work out a nice way to get different colours for multi-bar plots.

drawBarPlot :: (TypeableFloat n, Renderable (Path V2 n) b) => BarPlot b n -> Diagram b V2 n
drawBarPlot bp = foldMap makeBar (bp^.barPlotBars)
  where
    tW = bp^.barPlotWidth
    δ  = bp^.barPlotSpacing
    --
    makeBar (_,[]) = mempty
    makeBar (x,bs) = ifoldMap mkBar bs
      where
        mkBar i h = rect w h
                      # alignBL
                      # translateX (x + fromIntegral i * (δ + w))
        n = fromIntegral $ length bs
        w = recip n * (tW - δ * (n - 1))

    -- makeBar i [h1,h2] = rect (bW / 2) h1
    --                       # centerX
    --                       # alignB
    --                       # translateX (fromIntegral i + 1 - 0.15)
    --                  <> rect (bW / 2) h2
    --                       # fc orange
    --                       # centerX
    --                       # alignB
    --                       # translateX (fromIntegral i + 1 + 0.15)
    -- makeBar _ _ = mempty

-- instance (Typeable b, Renderable (Path R2) b) => Plotable (BarPlot b) b R2 where
--   plot _r _ t = transform t . drawBarPlot

simpleBarPlot :: (Renderable (Path V2 n) b, TypeableFloat n, Foldable f) => f n -> BarPlot b n
simpleBarPlot (toList -> xs) = def & barPlotBars .~ imap f xs 
  where
    f i h = (fromIntegral i + 1, [h])

_BarPlot :: Plotable (BarPlot b n) b => Prism' (Plot b V2 n) (BarPlot b n)
_BarPlot = _Plot



