{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TemplateHaskell    #-}
module Plots.Axis.Ticks where

import Control.Lens  hiding (transform, ( # ))
import Data.Default
import Data.Foldable
import Data.List     ((\\))
import Data.Ord
import Data.Typeable

import Diagrams.Prelude

import Diagrams.Coordinates.Traversals

data TickType = AutoTick
              | CentreTick
              | InsideTick
              | OutsideTick

-- | Function with access to the bounds of a coordinate.
type MajorTickFunction = (Double, Double) -> [Double]

-- | Function with access to the major ticks and bounds of a coordinate.
type MinorTickFunction = [Double]-> (Double, Double) -> [Double]

-- | Information for drawing ticks for a single coordinate.
data Ticks = Ticks
  { _majorTicksFun   :: MajorTickFunction
  , _minorTicksFun   :: MinorTickFunction
  , _majorTickType   :: TickType
  , _minorTickType   :: TickType
  , _majorTickLength :: Double
  , _minorTickLength :: Double
  , _majorTickStyle  :: Style R2
  , _minorTickStyle  :: Style R2
  } deriving Typeable

makeLenses ''Ticks

-- | Information for drawing ticks for a coordinate system.
type AxisTicks v = T v Ticks

instance Default Ticks where
  def = Ticks
          { _majorTicksFun   = niceTicks 7
          , _minorTicksFun   = minors 4
          , _majorTickType   = AutoTick
          , _minorTickType   = AutoTick
          , _majorTickLength = 5
          , _minorTickLength = 3
          , _minorTickStyle  = mempty # lwO 0.05
          , _majorTickStyle  = mempty # lwO 0.1
          }

-- functions for common ticks

noMajorTicksFunction :: MajorTickFunction
noMajorTicksFunction = const []

noMinorTicksFunction :: MinorTickFunction
noMinorTicksFunction _ = const []

-- if T = i * 10^j then log t = log i + j
-- this means if log t is an integer, t = 10^j

-- pgfplotsticks.code.tex lines: 1762, 1923, 2161

minors :: Double -> [Double] -> (Double, Double) -> [Double]
minors p xs@(x1:x2:_) (a,b) =
  filter (\n -> n > a + ε && n < b - ε)
         [x1 - 3*h, x1 - 2*h .. b]
   \\ xs
  where
    h = (x2 - x1) / p
    ε  = h * 0.1
minors _ _ _ = []

-- | Ticks whose value ends in 1, 0.5, 0.25, 0.2 (*10^n).
niceTicks :: Double -> (Double, Double) -> [Double]
-- niceTicks desiredTicks (a,b) = [i*h, (i + signum (b - a) ) * h .. b]
niceTicks desiredTicks (a,b) =
  filter (\n -> n > a + ε && n < b - ε)
         [i*h, (i + 1) * h .. b]
  where
    i = fromIntegral (truncate ( a / h ) :: Int)
    --
    d  = abs $ b - a
    ε  = h * 0.1
    h' = d / desiredTicks
    h  = minimumBy (comparing $ abs . (h' -)) allowedH
    --
    allowedH = map (* 10 ^^ (floor x :: Int)) allowedH'
    x        = logBase 10 d
    --
    allowedH' = [1, 0.5, 0.25, 0.2]


