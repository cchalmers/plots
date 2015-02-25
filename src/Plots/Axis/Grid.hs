{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Plots.Axis.Grid where

import Control.Lens hiding ((#))
import Data.Data
import Data.Default

import Diagrams.Prelude

-- | A grid line function takes the positions of the respective ticks
-- (minor ticks for minor grid lines, major ticks for major grid lines)
-- and the bounds of the axis and returns the positions of the grid
-- lines.
type GridLinesFunction n = [n] -> (n, n) -> [n]

data GridLines n = GridLines
  { _majorGridF     :: GridLinesFunction n
  , _minorGridF     :: GridLinesFunction n
  , _majorGridStyle :: Style V2 n
  , _minorGridStyle :: Style V2 n
  } deriving Typeable

makeLenses ''GridLines

type AxisGridLines v n = v (GridLines n)

instance (Typeable n, Floating n) => Default (GridLines n) where
  def = GridLines
          { _majorGridF    = tickGridF
          , _minorGridF    = noGridF
          , _majorGridStyle = mempty # lwO 0.4
          , _minorGridStyle = mempty # lwO 0.1
          }

-- | Place grid lines at the same position as the ticks.
tickGridF :: GridLinesFunction n
tickGridF = const

-- | No grid lines.
noGridF :: GridLinesFunction n
noGridF _ = const []

