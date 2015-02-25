{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DefaultSignatures       #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE ViewPatterns            #-}

module Diagrams.Coordinates.Isomorphic
  ( -- * Type constraints
    HasIndexedBasis, Euclidean

    -- * Vector like
  , VectorLike (..)
  , V2Like, V3Like

    -- * Point like
  , PointLike (..)
  , P2Like, P3Like
  )
  where

import           Control.Lens
import           Data.Complex
import           Data.Typeable

import           Diagrams.Prelude

type HasIndexedBasis v = (HasBasis v, TraversableWithIndex (E v) v)

-- | Umbrella class giving everything needed for working in the space. This is
--   basically a @V*@ from "linear".
type Euclidean v = (HasLinearMap v, HasIndexedBasis v, Metric v)

-- vector like ---------------------------------------------------------

-- | Provides an 'Iso\'' between @a@ and @v n@. This is normally used to
--   convert between the data type you're already using, @a@, and diagram's
--   native form, @v n@.
class (Euclidean v, Typeable v) => VectorLike v n a | a -> v n where
  -- | Isomorphism from @Point v n@ to something 'PointLike' @a@.
  --
  -- >>> V2 3 5 ^. vectorLike :: (Int, Int)
  -- (3,5)
  vectorLike :: Iso' (v n) a

  -- | Isomorphism from something 'PointLike' @a@ to @Point v n@.
  --
  -- >>> ((3, 5) :: (Int, Int)) ^. unvectorLike
  -- V2 3 5
  unvectorLike :: Iso' a (v n)
  unvectorLike = from vectorLike
  {-# INLINE unvectorLike #-}

instance VectorLike V2 n (V2 n) where
  vectorLike = id
  {-# INLINE vectorLike #-}

type V2Like = VectorLike V2

instance VectorLike V2 n (n, n) where
  vectorLike = iso unr2 r2
  {-# INLINE vectorLike #-}

instance VectorLike V2 n (Complex n) where
  vectorLike = iso (\(V2 x y) -> x :+ y)
                   (\(i :+ j) -> V2 i j)
  {-# INLINE vectorLike #-}

type V3Like = VectorLike V3

instance VectorLike V3 n (V3 n) where
  vectorLike = id
  {-# INLINE vectorLike #-}

instance VectorLike V3 n (n, n, n) where
  vectorLike = iso unr3 r3
  {-# INLINE vectorLike #-}

-- point like ----------------------------------------------------------

-- | Provides an 'Iso\'' between @a@ and @Point v n@. This is normally used to
--   convert between the data type you're already using, @a@, and diagram's
--   native form, @Point v n@.
class (Euclidean v, Typeable v) => PointLike v n a | a -> v n where
  -- | Isomorphism from @Point v n@ to something 'PointLike' @a@.
  --
  -- >>> mkP2 3 5 ^. pointLike :: (Int, Int)
  -- (3,5)
  pointLike :: Iso' (Point v n) a

  -- | Isomorphism from something 'PointLike' @a@ to @Point v n@.
  --
  -- >>> ((3, 5) :: (Int, Int)) ^. unpointLike
  -- P (V2 3 5)
  unpointLike :: Iso' a (Point v n)
  unpointLike = from pointLike
  {-# INLINE unpointLike #-}

-- | Things that are isomorphic to points in R2.
type P2Like = PointLike V2

instance PointLike V2 n (P2 n) where pointLike = id

instance PointLike V2 n (V2 n) where
  pointLike = iso (\(unp2 -> (x,y)) -> V2 x y)
                  (\(V2 x y)        -> x ^& y)
  {-# INLINE pointLike #-}

instance PointLike V2 n (n, n) where
  pointLike = iso unp2 p2
  {-# INLINE pointLike #-}

instance PointLike V2 n (Complex n) where
  pointLike = iso (\(unp2 -> (x,y)) -> x :+ y)
                  (\(i :+ j)        -> p2 (i,j))
  {-# INLINE pointLike #-}

type P3Like = PointLike V3

instance PointLike V3 n (P3 n) where
  pointLike = id
  {-# INLINE pointLike #-}

instance PointLike V3 n (n, n, n) where
  pointLike = iso unp3 p3
  {-# INLINE pointLike #-}

