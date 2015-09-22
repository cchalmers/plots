{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Plots.Utils
  ( liftRecommend
  , fromCommit
  , pathFromVertices
  , minmaxOf
  , enumFromToN

    -- * State helpers
  , (&=)
  , (&~~)
  ) where

import           Control.Lens
import           Control.Monad.State
import           Data.Monoid.Recommend
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
enumFromToN a b n = step n a
  where
    step !i !x | i < 1     = [x]
               | otherwise = x : step (i - 1) (x + diff)
    diff = (b - a) / fromIntegral n

-- | Apply a function over two recommends or two commits. If only one of
--   the values is a commit, the commit is used without applying the
--   function.
liftRecommend :: (a -> a -> a) -> Recommend a -> Recommend a -> Recommend a
liftRecommend _ (Commit a) (Recommend _)    = Commit a
liftRecommend _ (Recommend _) (Commit b)    = Commit b
liftRecommend f (Recommend a) (Recommend b) = Recommend (f a b)
liftRecommend f (Commit a) (Commit b)       = Commit (f a b)

-- | Extract a commit value, defaulting to the provided value when it is
--   a recommend.
fromCommit :: a -> Recommend a -> a
fromCommit _ (Commit a) = a
fromCommit a _          = a

------------------------------------------------------------------------
-- Diagrams
------------------------------------------------------------------------

-- | Type specialised version of 'fromVertices'.
pathFromVertices :: (Metric v, OrderedField n) => [Point v n] -> Path v n
pathFromVertices = fromVertices
{-# INLINE pathFromVertices #-}

-- | Minmax of a getter in the form @V2 min max@. Returns @(V2
--   (-Infinity) Infinity)@ for empty folds.
minmaxOf :: (Fractional a, Ord a) => Getting (Endo (Endo (V2 a))) s a -> s -> V2 a
minmaxOf l = foldlOf' l (\(V2 mn mx) a -> V2 (min mn a) (max mx a)) (V2 (1/0) (-1/0))
      -- (\acc a -> acc <**> V2 min max ?? a)
-- V2 is used instead of a tuple because V2 is strict.
{-# INLINE minmaxOf #-}

