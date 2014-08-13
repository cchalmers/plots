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

data BarPlot b = BarPlot
  { _barPlotBars     :: [[Double]]
  , _barPlotWidth    :: Double
  , _barPlotSpacing  :: Double
  -- , _barPlotInterval :: Double
  , _barPlotGeneric  :: GenericPlot b R2
  } deriving Typeable

type instance V (BarPlot b) = R2

makeLenses ''BarPlot

instance HasGenericPlot (BarPlot b) b R2 where
  genericPlot = barPlotGeneric

-- instance HasStyle (BarPlot b) where
--   applyStyle sty = over (genericPlot . themeLineStyle)   (applyStyle sty)
--                  . over (genericPlot . themeMarkerStyle) (applyStyle sty)
--                  . over (genericPlot . themeFillStyle)   (applyStyle sty)

instance Renderable (Path R2) b => Default (BarPlot b) where
  def = BarPlot
          { _barPlotBars     = []
          , _barPlotWidth    = 0.9
          , _barPlotSpacing  = 0.1
          -- , _barPlotInterval = 1
          , _barPlotGeneric  = def
          }

drawBarPlot :: Renderable (Path R2) b => BarPlot b -> Diagram b R2
drawBarPlot bp = ifoldMap makeBar (bp^.barPlotBars)
  where
    bW = bp^.barPlotWidth
    makeBar i [h] = rect bW h
                      -- # applyStyle sty
                      # centerX
                      # alignB
                      # translateX (fromIntegral i + 1)

    makeBar i [h1,h2] = rect (bW / 2) h1
                          # centerX
                          # alignB
                          # translateX (fromIntegral i + 1 - 0.15)
                     <> rect (bW / 2) h2
                          # fc orange
                          # centerX
                          # alignB
                          # translateX (fromIntegral i + 1 + 0.15)
    makeBar _ _ = mempty

instance (Typeable b, Renderable (Path R2) b) => Plotable (BarPlot b) b R2 where
  plot _r _ t = transform t . drawBarPlot

_BarPlot :: Plotable (BarPlot b) b R2 => Prism' (Plot b R2) (BarPlot b)
_BarPlot = _Plot



