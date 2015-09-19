{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE FlexibleContexts   #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Plots.Axis.Ticks
-- Copyright   :  (C) 2015 Christopher Chalmers
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Christopher Chalmers
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Ticks for being placed on an axis or a 'ColourBar'.
--
----------------------------------------------------------------------------
module Plots.Axis.Ticks
  ( -- * Major ticks
    MajorTicks
  , HasMajorTicks (..)
  , majorTicksHelper
  , logMajorTicks

    -- * Minor ticks
  , MinorTicks
  , HasMinorTicks (..)
  , minorTicksHelper

    -- * Both major and minor ticks
  , Ticks
  , HasTicks (..)
  , ticksAlign
  , ticksStyle
  , ticksVisible

    -- * Tick alignment
  , TickAlign (..)
  , autoTicks
  , centreTicks
  , centerTicks
  , insideTicks
  , outsideTicks
  ) where

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

-- Tick alignment ------------------------------------------------------

-- | Set the portion of the tick above and below the axis.
data TickAlign
  = TickSpec !Rational !Rational
  | AutoTick -- center tick for middle axis, outside tick otherwise
  deriving (Show, Eq)

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

-- -- | Do not show any ticks.
-- noTicks :: TickAlign
-- noTicks = NoTick

------------------------------------------------------------------------
-- Minor ticks
------------------------------------------------------------------------

-- | The big ticks on the axis line.
data MajorTicks v n = MajorTicks
  { matFunction :: (n,n) -> [n]
  , matAlign    :: TickAlign
  , matLength   :: n
  , matStyle    :: Style v n
  , matVisible  :: Bool
  }

instance (Enum n, TypeableFloat n) => Default (MajorTicks v n) where
  def = MajorTicks
    { matFunction = linearMajorTicks 6
    , matAlign    = autoTicks
    , matLength   = 5
    , matStyle    = mempty # lwO 0.4
    , matVisible  = True
    }

type instance V (MajorTicks v n) = v
type instance N (MajorTicks v n) = n

-- | Class of things that have a single 'MajorTicks'.
class HasMajorTicks a where
  -- | Lens onto the 'MajorTicks' of something.
  majorTicks :: Lens' a (MajorTicks (V a) (N a))

  -- | The function used to place ticks for this axis, given the bounds
  --   of the axis. The result of these major ticks are also used as
  --   guides for 'MinorTicks', 'MajorGridLines' and 'MinorGridLines'.
  --
  --   Default is @'linearMinorTicks' 5@.
  majorTickFunction :: Lens' a ((N a, N a) -> [N a])
  majorTickFunction = majorTicks . lens matFunction (\mat a -> mat {matFunction = a})

  -- | Alignment of the major ticks. Choose between 'autoTicks'
  --   (default), 'centreTicks', 'insideTicks' or 'outsideTicks'.
  majorTickAlign :: Lens' a TickAlign
  majorTickAlign = majorTicks . lens matAlign (\mat a -> mat {matAlign = a})

  -- | The total length the major ticks.
  --
  --   Default is @7@.
  majorTickLength :: Lens' a (N a)
  majorTickLength = majorTicks . lens matLength (\mat a -> mat {matLength = a})

  -- | The style used to render the major ticks.
  --
  --   Default is @'lwO' 0.6 'mempty'@ (subject to change).
  majorTickStyle :: Lens' a (Style (V a) (N a))
  majorTickStyle = majorTicks . lens matStyle (\mat sty -> mat {matStyle = sty})

  -- | Whether the major ticks should be 'visible'.
  --
  --   Default is 'True'.
  majorTickVisible :: Lens' a Bool
  majorTickVisible = majorTicks . lens matVisible (\mat b -> mat {matVisible = b})

instance HasMajorTicks (MajorTicks v n) where
  majorTicks = id

------------------------------------------------------------------------
-- Minor ticks
------------------------------------------------------------------------

-- | The small ticks on the axis line.
data MinorTicks v n = MinorTicks
  { mitFunction :: [n] -> (n,n) -> [n]
  , mitAlign    :: TickAlign
  , mitLength   :: n
  , mitStyle    :: Style v n
  , mitVisible  :: Bool
  }

type instance V (MinorTicks v n) = v
type instance N (MinorTicks v n) = n

instance (Enum n, TypeableFloat n) => Default (MinorTicks v n) where
  def = MinorTicks
    { mitFunction = linearMinorTicks 4
    , mitAlign    = autoTicks
    , mitLength   = 3
    , mitStyle    = mempty # lwO 0.4
    , mitVisible  = True
    }

-- | Class of things that have a single 'MinorTicks'.
class HasMinorTicks a where
  -- | Lens onto the 'MinorTicks' of something.
  minorTicks :: Lens' a (MinorTicks (V a) (N a))

  -- | The function used to place ticks for this axis, given the result
  --   of 'majorTicksFunction' and the bounds of the axis.
  --
  --   Default is @'linearMinorTicks' 3@.
  minorTickFunction :: Lens' a ([N a] -> (N a, N a) -> [N a])
  minorTickFunction = minorTicks . lens mitFunction (\mit a -> mit {mitFunction = a})

  -- | Alignment of the minor ticks. Choose between 'autoTicks'
  --   (default), 'centreTicks', 'insideTicks' or 'outsideTicks'.
  minorTickAlign :: Lens' a TickAlign
  minorTickAlign = minorTicks . lens mitAlign (\mit a -> mit {mitAlign = a})

  -- | The total length the minor ticks.
  --
  --   Default is @3@.
  minorTickLength :: Lens' a (N a)
  minorTickLength = minorTicks . lens mitLength (\mit a -> mit {mitLength = a})

  -- | The style used to render the minor ticks.
  --
  --   Default is @'lwO' 0.4 'mempty'@ (subject to change).
  minorTickStyle :: Lens' a (Style (V a) (N a))
  minorTickStyle = minorTicks . lens mitStyle (\mit sty -> mit {mitStyle = sty})

  -- | The style used to render the minor ticks.
  --
  --   Default is 'True'.
  minorTickVisible :: Lens' a Bool
  minorTickVisible = minorTicks . lens mitVisible (\mit sty -> mit {mitVisible = sty})

instance HasMinorTicks (MinorTicks v n) where
  minorTicks = id

------------------------------------------------------------------------
-- Both ticks
------------------------------------------------------------------------

-- | Both 'MajorTicks' and 'MinorTicks' together.
data Ticks v n = Ticks (MajorTicks v n) (MinorTicks v n)

type instance V (Ticks v n) = v
type instance N (Ticks v n) = n

-- | Class of things with both 'MajorTicks' and 'MinorTicks'.
class (HasMinorTicks a, HasMajorTicks a) => HasTicks a where
  bothTicks :: Lens' a (Ticks (V a) (N a))

instance HasTicks (Ticks v n) where
  bothTicks = id

instance HasMajorTicks (Ticks v n) where
  majorTicks f (Ticks ma mi) = f ma <&> \ma' -> Ticks ma' mi

instance HasMinorTicks (Ticks v n) where
  minorTicks f (Ticks ma mi) = f mi <&> \mi' -> Ticks ma mi'

instance (TypeableFloat n, Enum n) => Default (Ticks v n) where
  def = Ticks def def

instance Typeable n => HasStyle (Ticks v n) where
  applyStyle s = over ticksStyle (applyStyle s)

-- | Traversal over both major and minor tick alignment.
ticksAlign :: HasTicks a => Traversal' a TickAlign
ticksAlign f a = (\m mn -> a & majorTickAlign .~ m & minorTickAlign .~ mn)
                  <$> f (a ^. majorTickAlign) <*> f (a ^. minorTickAlign)

-- | Traversal over both major and minor tick styles.
ticksStyle :: HasTicks a => Traversal' a (Style (V a) (N a))
ticksStyle f a = (\m mn -> a & majorTickStyle .~ m & minorTickStyle .~ mn)
              <$> f (a ^. majorTickStyle) <*> f (a ^. minorTickStyle)

ticksVisible :: HasTicks a => Traversal' a Bool
ticksVisible f a = (\m mn -> a & majorTickVisible .~ m & minorTickVisible .~ mn)
              <$> f (a ^. majorTickVisible) <*> f (a ^. minorTickVisible)

-- functions for common ticks

-- noMajorTicksFunction :: MajorTickFunction n
-- noMajorTicksFunction = const []

-- noMinorTicksFunction :: MinorTickFunction n
-- noMinorTicksFunction _ = const []

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
majorTicksHelper ts0 n (a,b) = filter inRange hs where
  hs = [i*h, (i + 1) * h .. b]
  i  = fromIntegral (truncate ( a / h ) :: Int)

  -- -- We don't want the ticks touching the edge of the axis bounds so
  -- -- we discard any too close. This should be a parameter?
  inRange x = x >= a && x <= b

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
