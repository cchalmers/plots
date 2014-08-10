{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Plots.Types.Function
  ( FunctionPlot
  , mkFunctionPlot
    -- * Prism
  , _FunctionPlot

    -- * Lenses
  , functionPlotFunction
  , functionPlotNumPoints
  -- , functionPlotSmooth
  -- , functionPlotDiscontinuous
  ) where

import Control.Lens     hiding (transform, ( # ))
import Data.Default
import Data.Typeable
import Diagrams.Prelude hiding (view)
import Diagrams.Extra

import Plots.Themes
import Plots.Types
import Linear (el, ex)

data FunctionPlot b v = FunctionPlot
  { _functionPlotFunction      :: Double -> Point v
  , _functionPlotNumPoints     :: Int
  -- , _functionPlotSmooth        :: Bool
  -- , _functionPlotDiscontinuous :: Bool
  , _functionPlotGeneric       :: GenericPlot b v
  } deriving Typeable

type instance V (FunctionPlot b v) = v

makeLenses ''FunctionPlot

instance HasGenericPlot (FunctionPlot b v) b v where
  genericPlot = functionPlotGeneric

-- instance HasStyle (FunctionPlot b R2) where
--   applyStyle sty = over plotLineStyle   (applyStyle sty)
--                  . over plotMarkerStyle (applyStyle sty)
--                  . over plotFillStyle   (applyStyle sty)


instance Renderable (Path R2) b => Default (FunctionPlot b R2) where
  def = FunctionPlot
          { _functionPlotFunction      = const (mkP2 0 0)
          , _functionPlotNumPoints     = 100
          -- , _functionPlotSmooth        = False
          -- , _functionPlotDiscontinuous = False
          , _functionPlotGeneric       = def
          }

drawFunctionPlot :: Renderable (Path R2) b => T2 -> FunctionPlot b R2 -> Diagram b R2
drawFunctionPlot t fp = fromVertices functionPath
                          # transform t
                          # applyStyle (fp ^. themeLineStyle)
  where
    functionPath = map f [a, a + 1 / (fp ^. functionPlotNumPoints . to fromIntegral) .. b]
    f            = fp ^. functionPlotFunction
    a            = fp ^. plotBounds . el ex . lowerBound . recommend
    b            = fp ^. plotBounds . el ex . upperBound . recommend

instance (Typeable b, Renderable (Path R2) b) => Plotable (FunctionPlot b R2) b R2 where
  plot _ _ = drawFunctionPlot

_FunctionPlot :: Plotable (FunctionPlot b R2) b R2 => Prism' (Plot b R2) (FunctionPlot b R2)
_FunctionPlot = _Plot

mkFunctionPlot :: (Typeable b, Renderable (Path R2) b) => (Double -> Double) -> Plot b R2
mkFunctionPlot f = review _FunctionPlot $
  def & functionPlotFunction .~ f'
  where
    f' x = x ^& f x


