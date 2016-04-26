{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
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
  , TicksAlignment (..)
  , autoTicks
  , centreTicks
  , centerTicks
  , insideTicks
  , outsideTicks

    -- * Helper functions
  , hideTicks
  , majorTickPositions
  , minorTickPositions
  , linearMajorTicks
  , linearMinorTicks
  ) where

import           Control.Lens     hiding (transform, ( # ))
import           Data.Data
import           Data.Default
import           Data.Foldable    as F
import           Data.List        ((\\))
import           Data.Ord
import           Plots.Types
import           Plots.Util

import           Diagrams.Prelude

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

-- Tick alignment ------------------------------------------------------

-- | Set the portion of the tick above and below the axis.
data TicksAlignment
  = TickSpec !Rational !Rational
  | AutoTick -- center tick for middle axis, outside tick otherwise
  deriving (Show, Eq)

-- | Set the tick type depending on the axis line position. 'centreTick'
--   for 'middleAxis', 'insideTick' for everything else.
autoTicks :: TicksAlignment
autoTicks = AutoTick

-- | Set the tick to be in the centre of the axis with total length of
--   the corresponding tick length.
centreTicks :: TicksAlignment
centreTicks  = TickSpec 0.5 0.5

-- | Synonym for 'centreTicks'.
centerTicks :: TicksAlignment
centerTicks  = centreTicks

-- | Align the ticks to be inside a box axis.
insideTicks :: TicksAlignment
insideTicks  = TickSpec 0 1

-- | Align the ticks to be outside a box axis.
outsideTicks :: TicksAlignment
outsideTicks = TickSpec 1 0

-- -- | Do not show any ticks.
-- noTicks :: TicksAlignment
-- noTicks = NoTick

------------------------------------------------------------------------
-- Minor ticks
------------------------------------------------------------------------

-- | The big ticks on the axis line.
data MajorTicks v n = MajorTicks
  { matFunction :: (n,n) -> [n]
  , matAlign    :: TicksAlignment
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
class HasMajorTicks f a where
  -- | Lens onto the 'MajorTicks' of something.
  majorTicks :: LensLike' f a (MajorTicks (V a) (N a))

  -- | The function used to place ticks for this axis, given the bounds
  --   of the axis. The result of these major ticks are also used as
  --   guides for 'MinorTicks', 'MajorGridLines' and 'MinorGridLines'.
  --
  --   Default is @'linearMinorTicks' 5@.
  majorTicksFunction :: Functor f => LensLike' f a ((N a, N a) -> [N a])
  majorTicksFunction = majorTicks . lens matFunction (\mat a -> mat {matFunction = a})

  -- | Alignment of the major ticks. Choose between 'autoTicks'
  --   (default), 'centreTicks', 'insideTicks' or 'outsideTicks'.
  majorTicksAlignment :: Functor f => LensLike' f a TicksAlignment
  majorTicksAlignment = majorTicks . lens matAlign (\mat a -> mat {matAlign = a})

  -- | The total length the major ticks.
  --
  --   Default is @7@.
  majorTicksLength :: Functor f => LensLike' f a (N a)
  majorTicksLength = majorTicks . lens matLength (\mat a -> mat {matLength = a})

  -- | The style used to render the major ticks.
  --
  --   Default is @'lwO' 0.6 'mempty'@ (subject to change).
  majorTicksStyle :: Functor f => LensLike' f a (Style (V a) (N a))
  majorTicksStyle = majorTicks . lens matStyle (\mat sty -> mat {matStyle = sty})

instance HasMajorTicks f (MajorTicks v n) where
  majorTicks = id

instance HasVisibility (MajorTicks v n) where
  visible = lens matVisible (\mat b -> mat {matVisible = b})

------------------------------------------------------------------------
-- Minor ticks
------------------------------------------------------------------------

-- | The small ticks on the axis line.
data MinorTicks v n = MinorTicks
  { mitFunction :: [n] -> (n,n) -> [n]
  , mitAlign    :: TicksAlignment
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
class HasMinorTicks f a where
  -- | Lens onto the 'MinorTicks' of something.
  minorTicks :: LensLike' f a (MinorTicks (V a) (N a))

  -- | The function used to place ticks for this axis, given the result
  --   of 'majorTicksFunction' and the bounds of the axis.
  --
  --   Default is @'linearMinorTicks' 3@.
  minorTicksFunction :: Functor f => LensLike' f a ([N a] -> (N a, N a) -> [N a])
  minorTicksFunction = minorTicks . lens mitFunction (\mit a -> mit {mitFunction = a})

  -- | Alignment of the minor ticks. Choose between 'autoTicks'
  --   (default), 'centreTicks', 'insideTicks' or 'outsideTicks'.
  minorTicksAlignment :: Functor f => LensLike' f a TicksAlignment
  minorTicksAlignment = minorTicks . lens mitAlign (\mit a -> mit {mitAlign = a})

  -- | The total length the minor ticks.
  --
  --   Default is @3@.
  minorTicksLength :: Functor f => LensLike' f a (N a)
  minorTicksLength = minorTicks . lens mitLength (\mit a -> mit {mitLength = a})

  -- | The style used to render the minor ticks.
  --
  --   Default is @'lwO' 0.4 'mempty'@ (subject to change).
  minorTicksStyle :: Functor f => LensLike' f a (Style (V a) (N a))
  minorTicksStyle = minorTicks . lens mitStyle (\mit sty -> mit {mitStyle = sty})

instance HasMinorTicks f (MinorTicks v n) where
  minorTicks = id

instance HasVisibility (MinorTicks v n) where
  visible = lens mitVisible (\mit sty -> mit {mitVisible = sty})

------------------------------------------------------------------------
-- Both ticks
------------------------------------------------------------------------

-- | Both 'MajorTicks' and 'MinorTicks' together.
data Ticks v n = Ticks (MajorTicks v n) (MinorTicks v n)
-- Ticks are originally split up into major and minor so we can reuse
-- major for the colour bar. I'm still undecided whether it's worth all
-- the extra boilerplate here.

type instance V (Ticks v n) = v
type instance N (Ticks v n) = n

-- | Class of things with both 'MajorTicks' and 'MinorTicks'.
class (HasMinorTicks f a, HasMajorTicks f a) => HasTicks f a where
  bothTicks :: LensLike' f a (Ticks (V a) (N a))

instance Functor f => HasTicks f (Ticks v n) where
  bothTicks = id

instance Functor f => HasMajorTicks f (Ticks v n) where
  majorTicks f (Ticks ma mi) = f ma <&> \ma' -> Ticks ma' mi

instance Functor f => HasMinorTicks f (Ticks v n) where
  minorTicks f (Ticks ma mi) = f mi <&> \mi' -> Ticks ma mi'

instance (TypeableFloat n, Enum n) => Default (Ticks v n) where
  def = Ticks def def

instance Typeable n => HasStyle (Ticks v n) where
  applyStyle s = over ticksStyle (applyStyle s)

-- | Traversal over both major and minor tick alignment.
ticksAlign :: (HasTicks f a, Applicative f) => LensLike' f a TicksAlignment
ticksAlign = bothTicks . aligns
  where
    aligns f a = (\m mn -> a & majorTicksAlignment .~ m & minorTicksAlignment .~ mn)
                  <$> f (a ^. majorTicksAlignment) <*> f (a ^. minorTicksAlignment)

-- | Traversal over both major and minor tick styles.
ticksStyle :: (HasTicks f a, Applicative f) => LensLike' f a (Style (V a) (N a))
ticksStyle = bothTicks . styles
  where
    styles f a = (\m mn -> a & majorTicksStyle .~ m & minorTicksStyle .~ mn)
              <$> f (a ^. majorTicksStyle) <*> f (a ^. minorTicksStyle)

-- | Traversal over the visibility of both major and minor ticks.
ticksVisible :: (HasTicks f a, Applicative f) => LensLike' f a Bool
ticksVisible = bothTicks . visibles
  where
    visibles f a = (\m mn -> a & majorTicks . visible .~ m & minorTicks. visible .~ mn)
              <$> f (a ^. majorTicks . visible) <*> f (a ^. minorTicks . visible)

-- | Hides the 'Minor' ticks when trying to render something. This can
--   be used on multiple types:
--
-- @
-- 'hideTicks' :: 'Axis' b c n       -> 'Axis' b c n
-- 'hideTicks' :: 'SingleAxis' b v n -> 'SingleAxis' b v n
-- 'hideTicks' :: 'Ticks' v n        -> 'Ticks' v n
-- 'hideTicks' :: 'MinorTicks' v n   -> 'MinorTicks' v n
-- @
hideTicks :: HasTicks Identity a => a -> a
hideTicks = ticksVisible .~ False

-- | Setter over the final positions the major ticks. This is not as
--   general as 'majorTicksFunction' because you don't have access to
--   the bounds but it can be useful when you know exactly what ticks
--   you want to add or modify existing tick positions.
majorTickPositions
  :: (HasMajorTicks f a, Settable f)
  => LensLike' f a [N a]
majorTickPositions = majorTicksFunction . mapped

-- | Setter over the final positions the major ticks. This is not as
--   general as 'minorTicksFunction' because you don't have access to
--   the bounds but it can be useful when you know exactly what ticks
--   you want to add or modify existing tick positions.
minorTickPositions
  :: (HasMinorTicks f a, Settable f)
  => LensLike' f a [N a]
minorTickPositions = minorTicksFunction . mapped . mapped


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
