{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TemplateHaskell    #-}
module Plots.Axis.Ticks where

import Control.Lens  hiding (transform, ( # ))
import Data.Default
import Data.Foldable
import Data.List     ((\\))
import Data.Ord
import Data.Data

import Diagrams.Prelude

data TickType = AutoTick
              | CentreTick
              | InsideTick
              | OutsideTick

-- | Function with access to the bounds of a coordinate.
type MajorTickFunction n = (n, n) -> [n]

-- | Function with access to the major ticks and bounds of a coordinate.
type MinorTickFunction n = [n] -> (n, n) -> [n]

-- | Information for drawing ticks for a single coordinate.
data Ticks n = Ticks
  { _majorTicksFun   :: MajorTickFunction n
  , _minorTicksFun   :: MinorTickFunction n
  , _majorTickType   :: TickType
  , _minorTickType   :: TickType
  , _majorTickLength :: n
  , _minorTickLength :: n
  , _majorTickStyle  :: Style V2 n
  , _minorTickStyle  :: Style V2 n
  } deriving Typeable

makeLenses ''Ticks

-- | Information for drawing ticks for a coordinate system.
type AxisTicks v n = v (Ticks n)

instance (DataFloat n, Enum n) => Default (Ticks n) where
  def = Ticks
          { _majorTicksFun   = niceTicks 7
          , _minorTicksFun   = minors 4
          , _majorTickType   = AutoTick
          , _minorTickType   = AutoTick
          , _majorTickLength = 5
          , _minorTickLength = 3
          , _minorTickStyle  = mempty # lwO 0.4
          , _majorTickStyle  = mempty # lwO 0.6
          }

-- functions for common ticks

noMajorTicksFunction :: MajorTickFunction n
noMajorTicksFunction = const []

noMinorTicksFunction :: MinorTickFunction n
noMinorTicksFunction _ = const []

-- if T = i * 10^j then log t = log i + j
-- this means if log t is an integer, t = 10^j

-- pgfplotsticks.code.tex lines: 1762, 1923, 2161


minors :: (Enum n, Fractional n, Ord n) => n -> [n] -> (n, n) -> [n]
minors p xs@(x1:x2:_) (a,b) =
  filter (\n -> n > a + ε && n < b - ε)
         [x1 - 3*h, x1 - 2*h .. b] -- could get rid of Enum by doing this manually
   \\ xs
  where
    h = (x2 - x1) / p
    ε  = h * 0.1
minors _ _ _ = []

-- | Ticks whose value ends in 1, 0.5, 0.25, 0.2 (*10^n).
niceTicks :: (Enum n, RealFrac n, Floating n) => n -> (n, n) -> [n]
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


