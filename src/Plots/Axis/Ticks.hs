{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE FlexibleContexts   #-}
module Plots.Axis.Ticks where

import Control.Lens  hiding (transform, ( # ))
import Data.Default
import Data.Foldable
import Data.List     ((\\))
import Data.Ord
import Data.Data

import Diagrams.Prelude

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

-- | Low level type for determiniting length of tick below and above the
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
    { _majorTicksFun   = niceTicks 7
    , _minorTicksFun   = minors 4
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


minors :: (Enum n, Fractional n, Ord n) => n -> [n] -> (n, n) -> [n]
minors p xs@(x1:x2:_) (a,b) =
  filter (\n -> n > a + ε && n < b - ε)
         [x1 - 3*h, x1 - 2*h .. b] -- could get rid of Enum by doing this manually
   \\ xs
  where
    h = (x2 - x1) / p
    ε  = h * 0.1
minors _ _ _ = []

-- if T = i * 10^j then log t = log i + j
-- this means if log t is an integer, t = 10^j

-- | Ticks whose value ends in 1, 0.5, 0.25, 0.2 (*10^n).
niceTicks :: (Enum n, RealFrac n, Floating n) => n -> (n, n) -> [n]
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

-- -- | Generate a log axis automatically, scaled appropriate for the
-- -- input data.
-- autoScaledLogAxis :: RealFloat a => n -> (n,n) -> [n]
-- autoScaledLogAxis lap ps0 =
--     makeAxis' (realToFrac . log) (realToFrac . exp)
--               (_loga_labelf lap) (wrap rlabelvs, wrap rtickvs, wrap rgridvs)
--   where
--     ps        = filter (\x -> isValidNumber x && 0 < x) ps0
--     (minV,maxV) = (minimum ps,maximum ps)
--     wrap      = map fromRational
--     range []  = (3,30)
--     range _   | minV == maxV = (realToFrac $ minV/3, realToFrac $ maxV*3)
--               | otherwise    = (realToFrac $ minV,   realToFrac $ maxV)
--     (rlabelvs, rtickvs, rgridvs) = logTicks (range ps)

-- logTicks :: (Double,Double) -> ([Rational],[Rational],[Rational])
-- logTicks (low,high) = (major,minor,major)
--  where
--   pf :: RealFrac a => a -> (Integer, a)
--   pf = properFraction

--   -- frac :: (RealFrac a, Integral b) => a -> (b, a)
--   frac :: (RealFrac a) => a -> (Integer, a)
--   frac x | 0 <= b    = (a,b)
--          | otherwise = (a-1,b+1)
--     where
--       (a,b) = properFraction x

--   ratio      = high/low
--   lower a l  = let (i,r) = frac (log10 a) in
--                maximum (1:filter (\x -> log10 (fromRational x) <= r) l)*10^^i
--   upper a l  = let (i,r) = pf (log10 a) in
--                minimum (10:filter (\x -> r <= log10 (fromRational x)) l)*10^^i

--   powers           :: (Double,Double) -> [Rational] -> [Rational]
--   powers (x,y) l    = [ a*10^^p | p <- [(floor (log10 x))..(ceiling (log10 y))] :: [Integer]
--                                 , a <- l ]
--   midselection r l  = filter (inRange r l) (powers r l)
--   inRange (a,b) l x = (lower a l <= x) && (x <= upper b l)

--   logRange = (log10 low, log10 high)

--   roundPow x = 10^^(round x :: Integer)

--   major | 17.5 < log10 ratio = map roundPow $
--                                steps (min 5 (log10 ratio)) logRange
--         | 12 < log10 ratio   = map roundPow $
--                                steps (log10 ratio / 5) logRange
--         | 6 < log10 ratio    = map roundPow $
--                                steps (log10 ratio / 2) logRange
--         | 3 < log10 ratio    = midselection (low,high) [1,10]
--         | 20 < ratio         = midselection (low,high) [1,5,10]
--         | 6 < ratio          = midselection (low,high) [1,2,4,6,8,10]
--         | 3 < ratio          = midselection (low,high) [1..10]
--         | otherwise          = steps 5 (low,high)

--   (l',h')   = (minimum major, maximum major)
--   (dl',dh') = (fromRational l', fromRational h')
--   ratio' :: Double
--   ratio' = fromRational (h'/l')
--   filterX = filter (\x -> l'<=x && x <=h') . powers (dl',dh')

--   minor | 50 < log10 ratio' = map roundPow $
--                               steps 50 (log10 dl', log10 dh')
--         | 6 < log10 ratio'  = filterX [1,10]
--         | 3 < log10 ratio'  = filterX [1,5,10]
--         | 6 < ratio'        = filterX [1..10]
--         | 3 < ratio'        = filterX [1,1.2..10]
--         | otherwise         = steps 50 (dl', dh')


-- log10 :: Floating a => a -> a
-- log10 = logBase 10
