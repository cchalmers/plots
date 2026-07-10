{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Plots.Util
  ( pathFromVertices
  , minMaxOf
  , enumFromToN
  , whenever

    -- * State helpers
  , (&=)
  , (&~~)
  ) where

import           Control.Lens
import           Control.Monad.State
import           Data.Bool

import           Diagrams.Prelude           hiding (diff)

-- | Similar to '(%=)' but takes a state modification instead of a
--   function.
(&=) :: MonadState s m => ASetter' s b -> State b a -> m ()
l &= s = l %= execState s
infix 3 &=

-- | Similar to '(&~)' but works with 'StateT' and returns it in @m@.
(&~~) :: Monad m => s -> StateT s m a -> m s
l &~~ s = execStateT s l
infix 1 &~~

-- | @enumFromToN a b n@ calculates a list from @a@ to @b@ in @n@ steps.
enumFromToN :: Fractional n => n -> n -> Int -> [n]
enumFromToN a b n = go n a
  where
    go !i !x | i < 1     = [x]
             | otherwise = x : go (i - 1) (x + diff)
    diff = (b - a) / fromIntegral n

-- | Apply a function if the predicate is true.
whenever :: Bool -> (a -> a) -> a -> a
whenever b f = bool id f b

------------------------------------------------------------------------
-- Diagrams
------------------------------------------------------------------------

-- | Type specialised version of 'fromVertices'.
pathFromVertices :: (Metric v, OrderedField n) => [Point v n] -> Path v n
pathFromVertices = fromVertices
{-# INLINE pathFromVertices #-}

-- | Minmax of a getter in the form @V2 min max@. Returns @(V2
--   (-Infinity) Infinity)@ for empty folds.
minMaxOf :: (Fractional a, Ord a) => Getting (Endo (Endo (V2 a))) s a -> s -> (a,a)
minMaxOf l = foldlOf' l (\(V2 mn mx) a -> V2 (min mn a) (max mx a)) (V2 (1/0) (-1/0))
          <&> \(V2 x y) -> (x,y)
      -- (\acc a -> acc <**> V2 min max ?? a)
-- V2 is used instead of a tuple because V2 is strict.
{-# INLINE minMaxOf #-}

