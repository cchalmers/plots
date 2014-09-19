{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Plots.Axis.Grid where

import Control.Lens hiding ((#))
import Data.Data
import Data.Default

import Diagrams.Prelude


type GridLinesFunction n = [n] -> (n, n) -> [n]

data GridLines n = GridLines
  { _majorGridF     :: GridLinesFunction n
  , _minorGridF     :: GridLinesFunction n
  , _majorGridStyle :: Style V2 n
  , _minorGridStyle :: Style V2 n
  } deriving Typeable

makeLenses ''GridLines

type AxisGridLines v n = v (GridLines n)

instance (Data n, Floating n) => Default (GridLines n) where
  def = GridLines
          { _majorGridF    = tickGridF
          , _minorGridF    = noGridF
          , _majorGridStyle = mempty # lwO 0.4
          , _minorGridStyle = mempty # lwO 0.1
          }

tickGridF :: GridLinesFunction n
tickGridF = const

noGridF :: GridLinesFunction n
noGridF _ = const []

