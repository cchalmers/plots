{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE FlexibleContexts   #-}
module Plots.Axis.Ticks where

import Control.Lens  hiding (transform, ( # ))
import Data.Default
import Data.Foldable as F
import Data.List     ((\\))
import Data.Ord
import Data.Data
import Plots.Utils

import Diagrams.Prelude

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

-- | Low level type for determining length of tick below and above the
--   axis.
data TickAlign
  = TickSpec !Rational !Rational
  | AutoTick -- center tick for middle axis, outside tick otherwise
  | NoTick
  deriving (Show, Typeable)

-- | Set the tick type depending on the axis line position. 'centreTick'
--   for 'middleAxis', 'insideTick' for everything else.
autoTicks :: TickAlign
autoTicks = AutoTick

-- | Set the tick to be in the centre of the axis with total length of
--   the corresponding tick length.
centreTicks :: TickAlign
centreTicks  = TickSpec 0.5 0.5

-- | Synonym for 'centreTicks'.
centerTicks :: TickAlign
centerTicks  = centreTicks

-- | Align the ticks to be inside a box axis.
insideTicks :: TickAlign
insideTicks  = TickSpec 0 1

-- | Align the ticks to be outside a box axis.
outsideTicks :: TickAlign
outsideTicks = TickSpec 1 0

-- | Do not show any ticks.
noTicks :: TickAlign
noTicks = NoTick

-- | Function with access to the bounds of a coordinate.
type MajorTickFunction n = (n, n) -> [n]

-- | Function with access to the major ticks and bounds of a coordinate.
type MinorTickFunction n = [n] -> (n, n) -> [n]

-- | Information for drawing ticks for a single coordinate.
data Ticks v n = Ticks
  { _majorTicksFun   :: MajorTickFunction n
  , _majorTickAlign  :: TickAlign
  , _majorTickLength :: n
  , _majorTickStyle  :: Style v n
  , _minorTicksFun   :: MinorTickFunction n
  , _minorTickAlign  :: TickAlign
  , _minorTickLength :: n
  , _minorTickStyle  :: Style v n
  } deriving Typeable

type instance V (Ticks v n) = v
type instance N (Ticks v n) = n

makeLenses ''Ticks

-- | Information for drawing ticks for a coordinate system.
type AxisTicks v n = v (Ticks v n)

instance (TypeableFloat n, Enum n) => Default (Ticks v n) where
  def = Ticks
    { _majorTicksFun   = linearMajorTicks 7
    , _minorTicksFun   = linearMinorTicks 4
    , _majorTickAlign  = autoTicks
    , _minorTickAlign  = autoTicks
    , _majorTickLength = 5
    , _minorTickLength = 3
    , _minorTickStyle  = mempty # lwO 0.4
    , _majorTickStyle  = mempty # lwO 0.6
    }

instance Typeable n => HasStyle (Ticks v n) where
  applyStyle s = over tickStyles (applyStyle s)

-- | Traversal over both major and minor tick alignment.
tickAlign :: Traversal' (Ticks v n) TickAlign
tickAlign f a = (\m mn -> a & majorTickAlign .~ m & minorTickAlign .~ mn)
                  <$> f (a ^. majorTickAlign) <*> f (a ^. minorTickAlign)

-- | Traversal over both major and minor tick styles.
tickStyles :: Traversal' (Ticks v n) (Style v n)
tickStyles f a = (\m mn -> a & majorTickStyle .~ m & minorTickStyle .~ mn)
              <$> f (a ^. majorTickStyle) <*> f (a ^. minorTickStyle)

-- functions for common ticks

noMajorTicksFunction :: MajorTickFunction n
noMajorTicksFunction = const []

noMinorTicksFunction :: MinorTickFunction n
noMinorTicksFunction _ = const []

------------------------------------------------------------------------
-- Calculating ticks
------------------------------------------------------------------------

-- Linear ticks --------------------------------------------------------

-- | Ticks whose value ends in 1, 0.5, 0.25, 0.2 (*10^n).
linearMajorTicks :: (Enum n, RealFrac n, Floating n) => n -> (n, n) -> [n]
linearMajorTicks = majorTicksHelper [1, 0.5, 0.25, 0.2]

-- | Position @n@ minor ticks between each major tick.
linearMinorTicks :: (Enum n, Fractional n, Ord n) => n -> [n] -> (n, n) -> [n]
linearMinorTicks p xs@(x1:x2:_) (a,b) = filter inRange ts \\ xs where
  -- This whole things pretty hacky right now. Needs to be thought about
  -- and cleaned up.
  inRange n = n > a + ε && n < b - ε
  -- Could get rid of Enum by doing this manually.
  ts = [x1 - 3*h, x1 - 2*h .. b]
  h = (x2 - x1) / p
  ε  = h * 0.1
linearMinorTicks _ _ _ = []

-- Logarithmic ticks ---------------------------------------------------

-- | Place n ticks at powers of 10 on the axis.
logMajorTicks :: (Enum n, RealFrac n, Floating n) => n -> (n, n) -> [n]
logMajorTicks n (a,b) =
  -- Logarithmic ticks are just like linear ticks but in a different domain.
  map (10**) $ majorTicksHelper [1..9] n (log10 (max 2 a), log10 b)

-- Ticks helpers -------------------------------------------------------

-- | Place n linear spaced ticks between each major tick.
minorTicksHelper
  :: Fractional n
  => Int    -- ^ Number of minor ticks between each major tick
  -> [n]    -- ^ Positions of major ticks
  -> (n, n) -- ^ Bounds
  -> [n]    -- ^ Minor tick positions
minorTicksHelper n ts _ = F.concat $ go ts where
  -- we won't want x1 and x2 to be minor ticks too we init/tail them.
  go (x1:x2:xs) = (init . tail) (enumFromToN x1 x2 (n+2)) : go (x2:xs)
  go _          = []

-- | Choose ticks whose step size is a multiple of 10 of the allowed
--   numbers and tries to match the number of desired ticks.
majorTicksHelper
  :: (Enum n, RealFrac n, Floating n)
  => [n]    -- ^ Allowed numbers (up to powers of 10)
  -> n      -- ^ desired number of ticks
  -> (n, n) -- ^ bounds
  -> [n]    -- ^ tick positions
majorTicksHelper ts0 n (a,b) = hs where
  hs = [i*h, (i + 1) * h .. b]
  i  = fromIntegral (truncate ( a / h ) :: Int)

  -- -- We don't want the ticks touching the edge of the axis bounds so
  -- -- we discard any too close. This should be a parameter?
  -- inRange x = x > a + ε && x < b - ε
  -- ε         = h * 0.1

  -- Nice height that's closest to the height needed for desired number
  -- of ticks.
  h  = minimumBy (comparing $ abs . (h' -)) ts'

  -- Height for the desired number of ticks.
  h' = d / n

  -- Potential step heights that look nice and are in a suitable range
  -- for the axis bounds.
  ts' = map (* 10 ^^ (floor $ log10 d :: Int)) ts0
  d   = abs $ b - a

-- logged :: Floating a => Iso' a a
-- logged = iso log10 (10**)

log10 :: Floating a => a -> a
log10 = logBase 10
