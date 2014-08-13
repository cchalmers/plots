{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies       #-}
module Plots.Axis.Grid where

import Control.Lens hiding ((#))
import Data.Typeable
import Data.Default

import Diagrams.Prelude

import Diagrams.Coordinates.Traversals


type GridLinesFunction = [Double] -> (Double, Double) -> [Double]

data GridLines = GridLines
  { _majorGridF     :: GridLinesFunction
  , _minorGridF     :: GridLinesFunction
  , _majorGridStyle :: Style R2
  , _minorGridStyle :: Style R2
  } deriving Typeable

makeLenses ''GridLines

type AxisGridLines v = T v GridLines

instance Default GridLines where
  def = GridLines
          { _majorGridF    = tickGridF
          , _minorGridF    = noGridF
          , _majorGridStyle = mempty # lwO 0.4
          , _minorGridStyle = mempty # lwO 0.1
          }

tickGridF :: GridLinesFunction
tickGridF = const

noGridF :: GridLinesFunction
noGridF _ = const []

