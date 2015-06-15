{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Plots.Utils where

import           Control.Lens
import           Control.Lens.Internal
import           Control.Lens.Internal.Fold
import           Data.Foldable
import           Data.Monoid.Recommend
import           Data.Profunctor.Unsafe
import           Diagrams.Prelude           hiding (diff)
import           Linear

-- | @enumFromToN a b n@ calculates a list from @a@ to @b@ in @n@ steps.
enumFromToN :: Fractional n => n -> n -> Int -> [n]
enumFromToN a b n = step n a
  where
    step !i !x | i < 1     = [x]
               | otherwise = x : step (i - 1) (x + diff)
    diff = (b - a) / fromIntegral n

------------------------------------------------------------------------
-- Lens
------------------------------------------------------------------------

-- Index an optic, starting from 1.
oneindexing :: Indexable Int p => ((a -> Indexing f b) -> s -> Indexing f t) -> p a (f b) -> s -> f t
oneindexing l iafb s = snd $ runIndexing (l (\a -> Indexing (\i -> i `seq` (i + 1, indexed iafb i a))) s) 1
{-# INLINE oneindexing #-}

oneeach :: Each s t a b => IndexedTraversal Int s t a b
oneeach = conjoined each (indexing each)
{-# INLINE oneeach #-}

onefolded :: Foldable f => IndexedFold Int (f a) a
onefolded = conjoined folded' (indexing folded')
{-# INLINE onefolded #-}

folded' :: Foldable f => Fold (f a) a
folded' f = coerce . getFolding . foldMap (Folding #. f)
{-# INLINE folded' #-}

------------------------------------------------------------------------
-- Recommend
------------------------------------------------------------------------

liftRecommend :: (a -> a -> a) -> Recommend a -> Recommend a -> Recommend a
liftRecommend _ (Commit a) (Recommend _)    = Commit a
liftRecommend _ (Recommend _) (Commit b)    = Commit b
liftRecommend f (Recommend a) (Recommend b) = Recommend (f a b)
liftRecommend f (Commit a) (Commit b)       = Commit (f a b)

-- recommend :: Lens' (Recommend a) a
-- recommend = lens getRecommend setRecommend
--   where
--     setRecommend (Recommend _) a = Recommend a
--     setRecommend (Commit _   ) a = Commit a

-- _Recommend :: Prism' (Recommend a) a
-- _Recommend = prism' Recommend getRec
--   where
--     getRec (Recommend a) = Just a
--     getRec _             = Nothing

-- _Commit :: Prism' (Recommend a) a
-- _Commit = prism' Commit getCommit
--   where
--     getCommit (Commit a) = Just a
--     getCommit _          = Nothing

fromCommit :: a -> Recommend a -> a
fromCommit _ (Commit a) = a
fromCommit a _          = a

------------------------------------------------------------------------
-- Diagrams
------------------------------------------------------------------------

pathFromVertices :: (Metric v, OrderedField n) => [Point v n] -> Path v n
pathFromVertices = fromVertices

-- -- | The @themeEntry@ lens goes though recommend, so @set themeEntry myTheme
-- --   myPlot@ won't give a committed theme entry (so theme from axis will
-- --   override). Use commitTheme to make sure theme is committed.
-- commitTheme :: HasPlotProperties a => ThemeEntry (B a) (N a) -> a -> a
-- commitTheme = set plotThemeEntry . Commit
--
-- -- | Make the current theme a committed theme. See @commitTheme@.
-- commitCurrentTheme :: HasPlotProperties a => a -> a
-- commitCurrentTheme = over plotThemeEntry makeCommitted
--   where
--     makeCommitted (Recommend a) = Commit a
--     makeCommitted c             = c
