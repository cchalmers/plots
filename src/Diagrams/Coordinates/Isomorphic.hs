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
  , R2Like, r2Like, r2Iso
  , R3Like, r3Like, r3Iso

    -- * Point like
  , PointLike (..)
  , P2Like, p2Like, p2Iso
  , P3Like, p3Like, p3Iso
  )
  where

import           Control.Lens
import           Data.Complex
import           Data.Typeable

import           Diagrams.Prelude
import           Diagrams.ThreeD.Types
import           Diagrams.Core.Transform

type HasIndexedBasis v = (HasBasis v, TraversableWithIndex (E v) v)

type Euclidean v = (HasLinearMap v, HasIndexedBasis v, Metric v)


-- | An isomorphism between a (possiblly monomorphic) type @a@ and euclidean 
--   vector @v@ under numerical field n.
class (Euclidean v, Typeable v) => VectorLike v n a | a -> v n where
  vectorLike :: Iso' (v n) a

  unvectorLike :: Iso' a (v n)
  unvectorLike = from vectorLike

instance VectorLike V2 n (V2 n) where vectorLike = id

type R2Like = VectorLike V2

r2Like :: R2Like n a => Iso' (V2 n) a
r2Like = vectorLike

r2Iso :: R2Like n a => Iso' a (V2 n)
r2Iso = unvectorLike

instance VectorLike V2 n (n, n) where
  vectorLike = iso unr2 r2

instance VectorLike V2 n (Complex n) where
  vectorLike = iso (\(unr2 -> (x,y)) -> x :+ y)
                   (\(i :+ j)        -> r2 (i,j))

type R3Like = VectorLike V3

instance VectorLike V3 n (V3 n) where vectorLike = id

r3Like :: R3Like n a => Iso' (V3 n) a
r3Like = vectorLike

instance VectorLike V3 n (n, n, n) where
  vectorLike = iso unr3 r3

-- | Some @a@ which is isomorphic to a point in vector space @v@.
class (Euclidean v, Typeable v) => PointLike v n a | a -> v n where
  pointLike :: Iso' (Point v n) a

  unpointLike :: Iso' a (Point v n)
  unpointLike = from pointLike

-- | Things that are isomorphic to points in R2.
type P2Like = PointLike V2

instance PointLike V2 n (P2 n) where pointLike = id

p2Like :: P2Like n a => Iso' (P2 n) a
p2Like = pointLike

p2Iso :: P2Like n a => Iso' a (P2 n)
p2Iso = unpointLike

instance PointLike V2 n (V2 n) where
  pointLike = iso (\(unp2 -> (x,y)) -> V2 x y)
                  (\(V2 x y)        -> x ^& y)

instance PointLike V2 n (n, n) where
  pointLike = iso unp2 p2

instance PointLike V2 n (Complex n) where
  pointLike = iso (\(unp2 -> (x,y)) -> x :+ y)
                  (\(i :+ j)        -> p2 (i,j))


type P3Like = PointLike V3

-- instance PointLike V3 n (P3 n) where pointLike = id

p3Like :: P3Like n a => Iso' (P3 n) a
p3Like = pointLike

instance PointLike V3 n (n, n, n) where
  pointLike = iso unp3 p3

