{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DefaultSignatures       #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE ViewPatterns            #-}

module Diagrams.Coordinates.Isomorphic where

import Control.Lens

import           Data.Complex
import           Diagrams.Prelude2
import qualified Linear.Affine     as L

import Data.Basis

type DiagramsScalar s =
  (RealFloat s,
   VectorSpace s,
   Transformable s,
   HasBasis s,
   -- for bounding box
   Ord s,
   AdditiveGroup s,
   Typeable s)

type DiagramsCoordinate v =
  (HasBasis v,
   Ord (Basis v),
   -- my ghc-mod doesn't like these in types
   -- V v ~ v,
   Transformable v,
   InnerSpace v,
   Coordinates v,
   DiagramsScalar (Scalar v),
   HasLinearMap v,
   Typeable v)

-- Isomorphic to a vector

-- | Some @a@ which is isomorphic to vector @v@.
class DiagramsCoordinate v => VectorLike a v | a -> v where
  vectorLike :: Iso' v a
  -- default diaVector Iso' v v
  -- vectorLike = id

  diaVector :: Iso' a v
  diaVector = from vectorLike

instance VectorLike R2 R2 where
  vectorLike = id

instance VectorLike R3 R3 where
  vectorLike = id

type R2Like a = VectorLike a R2

r2Like :: R2Like a => Iso' R2 a
r2Like = vectorLike

r2Iso :: R2Like a => Iso' a R2
r2Iso = diaVector

instance VectorLike (V2 Double) R2 where
  vectorLike = iso (\(unr2 -> (x,y)) -> V2 x y)
                  (\(V2 x y)        -> x ^& y)

instance VectorLike (Double, Double) R2 where
  vectorLike = iso unr2 r2

instance VectorLike (Complex Double) R2 where
  vectorLike = iso (\(unr2 -> (x,y)) -> x :+ y)
                  (\(i :+ j)        -> r2 (i,j))

type R3Like a = VectorLike a R3

r3Like :: R3Like a => Iso' R3 a
r3Like = vectorLike

instance VectorLike (Double, Double, Double) R3 where
  vectorLike = iso unr3 r3

instance VectorLike (V3 Double) R3 where
  vectorLike = iso (\(unr3 -> (x,y,z)) -> V3 x y z)
                  (\(V3 x y z)        -> x ^& y ^& z)

-- Isomorphic to a Point

-- | Some @a@ which is isomorphic to coordinate @v@.
class DiagramsCoordinate v => PointLike a v | a -> v where
  pointLike :: Iso' (Point v) a

  diaPoint :: Iso' a (Point v)
  diaPoint = from pointLike

instance PointLike P2 R2 where
  pointLike = id

instance PointLike P3 R3 where
  pointLike = id

type P2Like a = PointLike a R2

p2Like :: P2Like a => Iso' P2 a
p2Like = pointLike

p2Iso :: P2Like a => Iso' a P2
p2Iso = diaPoint

-- | @linear@ appears to be more relaxed in its distinction between points and
--   vectors.
instance PointLike (V2 Double) R2 where
  pointLike = iso (\(unp2 -> (x,y)) -> V2 x y)
                  (\(V2 x y)        -> x ^& y)

instance PointLike (L.Point V2 Double) R2 where
  pointLike = iso (\(unp2 -> (x,y)) -> L.P (V2 x y))
                  (\(L.P (V2 x y))  -> x ^& y)

instance PointLike (Double, Double) R2 where
  pointLike = iso unp2 p2

instance PointLike (Complex Double) R2 where
  pointLike = iso (\(unp2 -> (x,y)) -> x :+ y)
                  (\(i :+ j)        -> p2 (i,j))

type P3Like a = PointLike a R3

p3Like :: P3Like a => Iso' P3 a
p3Like = pointLike

instance PointLike (Double, Double, Double) R3 where
  pointLike = iso unp3 p3

instance PointLike (V3 Double) R3 where
  pointLike = iso (\(unp3 -> (x,y,z)) -> V3 x y z)
                  (\(V3 x y z)        -> x ^& y ^& z)

