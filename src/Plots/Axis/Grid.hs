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
  { _majorGridLineFun   :: GridLinesFunction
  , _minorGridLineFun   :: GridLinesFunction
  , _majorGridLineStyle :: Style R2
  , _minorGridLineStyle :: Style R2
  } deriving Typeable

makeLenses ''GridLines

type AxisGridLines v = T v GridLines

instance Default GridLines where
  def = GridLines
          { _majorGridLineFun  = const
          , _minorGridLineFun  = const
          , _majorGridLineStyle = mempty # lwO 0.4
          , _minorGridLineStyle = mempty # lwO 0.1
          }

tickGridLinesFunction :: GridLinesFunction
tickGridLinesFunction = const

