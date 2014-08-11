{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ViewPatterns           #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE ConstrainedClassMethods #-}

module Diagrams.Coordinates.Isomorphic where

import Control.Lens

import Diagrams.Prelude2
import Data.Complex

-- Isomorphic to R2

-- | Some @a@ which is isomorphic to coordinate @v@.
class CoordinateLike a v | a -> v where
  coordLike :: Iso' v a
  -- default diaCoord Iso' v v
  -- coordLike = id

  diaCoord :: Iso' a v
  diaCoord = from coordLike

instance CoordinateLike R2 R2 where
  coordLike = id

instance CoordinateLike R3 R3 where
  coordLike = id

type R2Like a = CoordinateLike a R2

r2Like :: R2Like a => Iso' R2 a
r2Like = coordLike

r2Iso :: R2Like a => Iso' a R2
r2Iso = diaCoord

instance CoordinateLike (V2 Double) R2 where
  coordLike = iso (\(unr2 -> (x,y)) -> V2 x y)
                  (\(V2 x y)        -> x ^& y)

instance CoordinateLike (Double, Double) R2 where
  coordLike = iso unr2 r2

instance CoordinateLike (Complex Double) R2 where
  coordLike = iso (\(unr2 -> (x,y)) -> x :+ y)
                  (\(i :+ j)        -> r2 (i,j))

type R3Like a = CoordinateLike a R3

r3Like :: R3Like a => Iso' R3 a
r3Like = coordLike

instance CoordinateLike (Double, Double, Double) R3 where
  coordLike = iso unr3 r3

instance CoordinateLike (V3 Double) R3 where
  coordLike = iso (\(unr3 -> (x,y,z)) -> V3 x y z)
                  (\(V3 x y z)        -> x ^& y ^& z)

