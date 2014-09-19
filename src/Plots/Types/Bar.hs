{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Plots.Types.Bar
  ( BarPlot
    -- * Prism
  , _BarPlot

    -- * Lenses
  , barPlotBars
  , barPlotWidth
  , barPlotSpacing
  ) where

import Control.Lens     hiding (transform, ( # ))
import Data.Default
import Data.Typeable
import Diagrams.Prelude

-- import Plots.Themes
import Plots.Types

data BarPlot b n = BarPlot
  { _barPlotBars     :: [[n]]
  , _barPlotWidth    :: n
  , _barPlotSpacing  :: n
  -- , _barPlotInterval :: n
  , _barPlotGeneric  :: GenericPlot b V2 n
  } deriving Typeable

type instance V (BarPlot b n) = V2
type instance N (BarPlot b n) = n

makeLenses ''BarPlot

instance HasGenericPlot (BarPlot b n) b where
  genericPlot = barPlotGeneric

-- instance HasStyle (BarPlot b) where
--   applyStyle sty = over (genericPlot . themeLineStyle)   (applyStyle sty)
--                  . over (genericPlot . themeMarkerStyle) (applyStyle sty)
--                  . over (genericPlot . themeFillStyle)   (applyStyle sty)

instance (TypeableFloat n, Renderable (Path V2 n) b) => Default (BarPlot b n) where
  def = BarPlot
          { _barPlotBars     = []
          , _barPlotWidth    = 0.9
          , _barPlotSpacing  = 0.1
          -- , _barPlotInterval = 1
          , _barPlotGeneric  = def
          }

-- drawBarPlot :: Renderable (Path R2) b => BarPlot b -> Diagram b R2
-- drawBarPlot bp = ifoldMap makeBar (bp^.barPlotBars)
--   where
--     bW = bp^.barPlotWidth
--     makeBar i [h] = rect bW h
--                       -- # applyStyle sty
--                       # centerX
--                       # alignB
--                       # translateX (fromIntegral i + 1)
-- 
--     makeBar i [h1,h2] = rect (bW / 2) h1
--                           # centerX
--                           # alignB
--                           # translateX (fromIntegral i + 1 - 0.15)
--                      <> rect (bW / 2) h2
--                           # fc orange
--                           # centerX
--                           # alignB
--                           # translateX (fromIntegral i + 1 + 0.15)
--     makeBar _ _ = mempty

-- instance (Typeable b, Renderable (Path R2) b) => Plotable (BarPlot b) b R2 where
--   plot _r _ t = transform t . drawBarPlot

_BarPlot :: Plotable (BarPlot b n) b => Prism' (Plot b V2 n) (BarPlot b n)
_BarPlot = _Plot



