{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE CPP         #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Projections
-- Copyright   :  (c) 2014
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  c.chalmers@me.com
--
-- Linear maps are somewhere between deformations and transformations.
--
-- Linear maps are a superset of transformations in that they need not be
-- invertible and can go to different vector spaces.
-- 
-- Linear maps are a subset of deformations because they have to be linear. 
-- This increases the domain of things we can map (

-- 
-- (Note: information is from Wikipedia and not guaranteed to be accurate.)
--
-----------------------------------------------------------------------------

module Diagrams.LinearMap
  ( -- * Linear map
  --   (:-*)
  -- , linear
  -- , lapply

    -- * LinearMap-able class
    LinearMappable (..)
  -- , asLinearMap
  -- , extractYZ
  ) where

import Control.Lens     hiding (at, transform, lmap)
-- import Data.AffineSpace
-- import Data.Basis
-- import Data.MemoTrie
-- import Data.Monoid      hiding ((<>))
-- import Data.Semigroup
-- import Data.Typeable

import Diagrams.Core
import Diagrams.Located
-- import Diagrams.Parametric
import Diagrams.Path
import Diagrams.Segment
import Diagrams.Trail
import Diagrams.TwoD
import Diagrams.ThreeD.Types

import qualified Data.FingerTree     as FT

import Linear.Affine
import Diagrams.Prelude

offsetVector :: Traversal (Offset c v n) (Offset c v' n') (v n) (v' n')
offsetVector f (OffsetClosed v) = OffsetClosed <$> f v
offsetVector _ OffsetOpen       = pure OffsetOpen

------------------------------------------------------------
-- Deformations

class LinearMappable u v where

  -- | Apply a linear map to something @LinearMappable@.
  lmap  :: (N u ~ n, N v ~ n) => (Vn u -> Vn v) -> u -> v
-- #ifdef HLINT
--   default lmap :: (Scalar u ~ Scalar v, HasLinearMap u, HasLinearMap v)
--                => (u :-* v) -> u -> v
--   lmap = lapply
-- #endif

-- | @asLinearMap@ converts a 'Transformation' to a linear map by
-- discarding the inverse transform.  This allows reusing
-- @Transformation@s in the construction of @Transfiguration@s.
-- asLinearMap :: HasLinearMap v => Transformation v -> v :-* v
-- asLinearMap = apply

------------------------------------------------------------
-- Instances

-- stuff

instance LinearMappable (V2 n) (V2 n) where lmap = id
instance LinearMappable (V2 n) (V3 n) where lmap = id
instance LinearMappable (V3 n) (V2 n) where lmap = id
instance LinearMappable (V3 n) (V3 n) where lmap = id

-- containers

instance LinearMappable u v => LinearMappable [u] [v] where
  lmap = map . lmap

-- Points

instance (HasLinearMap u, HasLinearMap v)
  => LinearMappable (Point u n) (Point v n) where
  lmap f (P v) = P $ f v

-- paths and path-like things

instance (HasLinearMap u, HasLinearMap v)
  => LinearMappable (FixedSegment u n) (FixedSegment v n) where
  lmap l s = case s of
    FLinear p0 p1      -> FLinear (f p0) (f p1)
    FCubic p0 c1 c2 p1 -> FCubic (f p0) (f c1) (f c2) (f p1)
    where f = lmap l

instance (LinearMappable (u n) (v n), HasLinearMap u, HasLinearMap v)
  => LinearMappable (Offset c u n) (Offset c v n) where
  lmap = over offsetVector

instance (HasLinearMap v, HasLinearMap u)
  => LinearMappable (Segment c v n) (Segment c u n) where
  lmap = mapSegmentVectors

instance (HasLinearMap (V a), Metric (V a), OrderedField (N a), FT.Measured m a, LinearMappable a b, FT.Measured m b)
  => LinearMappable (FT.FingerTree m a) (FT.FingerTree m b) where
  lmap = FT.fmap' . lmap

instance (OrderedField n, HasLinearMap a, Metric a, HasLinearMap b, Metric b)
  => LinearMappable (SegTree a n) (SegTree b n) where
  lmap l = over _Wrapped (FT.fmap' . lmap $ l)

instance (OrderedField n, HasLinearMap v, Metric v, HasLinearMap u, Metric u)
    => LinearMappable (Trail' l v n) (Trail' l u n) where
  lmap l (Line t  ) = Line (lmap l t)
  lmap l (Loop t s) = Loop (lmap l t) (lmap l s)

instance (N a ~ N b, LinearMappable a b, HasLinearMap (V a), HasLinearMap (V b))
  => LinearMappable (Located a) (Located b) where
  lmap l (viewLoc -> (p, a)) = lmap l a `at` lmap l p

instance (HasLinearMap v, Metric v, OrderedField n,
          HasLinearMap u, Metric u)
    => LinearMappable (Trail v n) (Trail u n) where
  lmap l = onTrail' (lmap l) (lmap l)
    where
      onTrail' o c = withTrail (wrapLine . o) (wrapLoop . c)

instance (HasLinearMap v, Metric v, OrderedField n,
          HasLinearMap u, Metric u)
    => LinearMappable (Path v n) (Path u n) where
  lmap l = over _Wrapped (lmap l)
    


-- deriving instance
--   (HasLinearMap (V a), InnerSpace (V a), OrderedField (Scalar (V a)),
--    FT.Measured m a, LinearMappable a b, FT.Measured m b)
--   => LinearMappable (SegTree a) (SegTree b)

-- linear mappings on linear maps

-- type instance V (u :-: u) = u
-- type instance V (u :-* u) = u

-- instance LinearMappable (T.R3 :-* T.R3) (R2 :-* R2) where
--   lmap l l3 = linear f
--     where
--       f (D.unr2 -> (y,z)) = lapply l $ lapply l3 (T.r3 (0, y, z))
-- 
--   
-- -- I'm not sure if it makes sense to apply a linear map to an automorphism.
-- instance (Scalar u ~ Double, HasLinearMap u, LinearMappable (u :-* u) (R2 :-* R2))
--   => LinearMappable (u :-: u) (R2 :-: R2) where
--   lmap l (a :-: _) = withInvertedR2 (lmap l a)
-- 
-- withInvertedR2 :: (R2 :-* R2) -> R2 :-: R2
-- withInvertedR2 l = l :-: l'
--   where
--     l' = if det < 1e-8
--            then error "non-invertible transform"
--            else recip det *^ linear lInv
--     det = a*d - b*c
--     lInv (unr2 -> (x,y)) = (d*x - b*y) ^& (-c*x + a*y)
--     (a,b) = unr2 $ lapply l unitX
--     (c,d) = unr2 $ lapply l unitY
-- 
-- 
-- instance (u ~ V u, Scalar u ~ Double, HasLinearMap u,
--           LinearMappable (u :-: u) (R2 :-: R2))
--   => LinearMappable (Transformation u) T2 where
--   lmap l (Transformation a aT v) =
--     Transformation (lmap l a) (lmap l aT) (lapply l v)




-- instance (Scalar u ~ Scalar v, HasLinearMap u, HasLinearMap v)
--   => LinearMappable (Transformation u) (Transformation v) where
--   lmap l (Transformation (a :-: b) (a' :-: b') v) =
--     Transformation 



-- linearXTruncation :: T.R3 :-* D.R2
-- linearXTruncation = linear f
--   where
--     f (T.unr3 -> (_,y,z)) = D.r2 (y,z)
-- 
-- truncateLinearMap :: (T.R3 :-* T.R3) -> (D.R2 :-* D.R2)
-- truncateLinearMap l3 = linear f
--   where
--     f (D.unr2 -> (y,z))   = t $ lapply l3 (T.r3 (0, y, z))
--     t (T.unr3 -> (_,y,z)) = D.r2 (y,z)
-- 
-- extractYZ :: T.T3 -> D.T2
-- extractYZ (Transformation (s :-: t) (a :-: b) v) = 
--   Transformation (f s :-: f t) (f a :-: f b) (lapply linearXTruncation v)
--   where
--     f = truncateLinearMap





-- 
-- -- | Cubic curves are not closed under perspective projections.
-- --   Therefore @Segment@s are not an instance of Transfigureable.  However,
-- --   the Transfiguration of a @Segment@ can be approximated to arbitrary
-- --   precision by a series of @Segment@s.  @deformSegment@ does this,
-- --   which allows types built from lists of @Segment@s to themselves be
-- --   @Transfigureable@.
-- deformSegment :: (VectorSpace v1,
--                   InnerSpace v1,
--                   VectorSpace v2,
--                   InnerSpace v2,
--                   Scalar v1 ~ Double,
--                   Scalar v2 ~ Double)
--               => Double -> Transfiguration v1 v2 -> FixedSegment v1 -> [FixedSegment v2]
-- -- deformSegment epsilon t s
-- deformSegment epsilon t = go (0::Int)
--   where
--     go n s
--       | n == 100               = [approx t s]
--       | goodEnough epsilon t s = [approx t s]
--       | otherwise              = concatMap (deformSegment epsilon t) [s1, s2]
--       where
--         (s1, s2) = splitAtParam s 0.5
--   -- | goodEnough epsilon t s = [approx t s]
--   -- | otherwise              = concatMap (deformSegment epsilon t) [s1, s2]
--   -- where
--   --   (s1, s2) = splitAtParam s 0.5
-- 
-- approx :: Transfiguration v1 v2 -> FixedSegment v1 -> FixedSegment v2
-- approx t (FLinear p0 p1)      = FLinear (deform t p0) (deform t p1)
-- approx t (FCubic p0 c1 c2 p1) = FCubic (f p0) (f c1) (f c2) (f p1)
--   where
--     f = deform t
-- 
-- goodEnough :: (InnerSpace v2, VectorSpace v1, Ord (Scalar v1),
--                      Floating (Scalar v1), Scalar v2 ~ Scalar v1) =>
--               Scalar v1 -> Transfiguration v1 v2 -> FixedSegment v1 -> Bool
-- goodEnough e t s =
--   all (< e) [magnitude $ deform t (s `atParam` u) .-. approx t s `atParam` u
--             | u <- [0.25, 0.5, 0.75]]
-- 
-- instance (VectorSpace v1,
--           InnerSpace v1,
--           VectorSpace v2,
--           InnerSpace v2,
--           Scalar v1 ~ Double,
--           Scalar v2 ~ Double)
--          => Transfigureable (Located (Trail v1)) (Located (Trail v2)) where
--   deform' eps p t
--     | isLine $ unLoc t  = line `at` p0
--     | otherwise         = glueTrail line `at` p0
--     where
--       segs = concatMap (deformSegment eps p) $ fixTrail t
--       p0   = case segs of
--                (FLinear start _:_)    -> start
--                (FCubic start _ _ _:_) -> start
--                _                      -> deform p (loc t)  -- default in case of empty trail
--       line = trailFromSegments $ map (unLoc . fromFixedSeg) segs
-- 
--   deform p t = deform' (0.01 * extent) p t
--     where
--       -- estimate the "size" of the Trail' as the maximum distance to
--       -- any vertex
--       extent  = maximum . map dist . trailVertices $ t
--       dist pt = magnitude $ pt .-. loc t
-- 
-- instance (VectorSpace v1, InnerSpace v1, VectorSpace v2, InnerSpace v2,
--           Scalar v1 ~ Double, Scalar v2 ~ Double)
--          => Transfigureable (Path v1) (Path v2) where
--   deform' eps p = over (_Wrapped . traversed) (deform' eps p)
--   deform p      = over (_Wrapped . traversed) (deform p)
-- 
-- -- instance Transfigureable T3 T2 where
-- --   deform' = const deform
-- --   deform (Transfiguration d) (Transformation (s :-: t) (a :-: b) v = 
-- 
-- linearXTruncation :: T.R3 :-* D.R2
-- linearXTruncation = linear f
--   where
--     f (T.unr3 -> (_,y,z)) = D.r2 (y,z)
-- 
-- truncateLinearMap :: (T.R3 :-* T.R3) -> (D.R2 :-* D.R2)
-- truncateLinearMap l3 = linear f
--   where
--     f (D.unr2 -> (y,z))   = t $ lapply l3 (T.r3 (0, y, z))
--     t (T.unr3 -> (_,y,z)) = D.r2 (y,z)
-- 
-- extractYZ :: T.T3 -> D.T2
-- extractYZ (Transformation (s :-: t) (a :-: b) v) = 
--   Transformation (f s :-: f t) (f a :-: f b) (lapply linearXTruncation v )
--   where
--     f = truncateLinearMap
-- 
-- 
-- 
-- -- Wrapper for different deform instance that only affects the position of the text
-- newtype TranslateOnly a = TranslateOnly a
--   deriving (Typeable, Functor)
-- 
-- type instance V (TranslateOnly a) = V a
-- 
-- instance Transformable a => Transformable (TranslateOnly a) where
--   transform t = fmap (transform t)
-- 
-- instance HasStyle a => HasStyle (TranslateOnly a) where
--   applyStyle s = fmap (applyStyle s)
-- 
-- instance Rewrapped (TranslateOnly a) (TranslateOnly b)
-- instance Wrapped (TranslateOnly a) where
--   type Unwrapped (TranslateOnly a) = a
--   _Wrapped' = iso (\(TranslateOnly a) -> a) TranslateOnly
-- 
-- 
-- 
-- -- transfigurate arbitrary diagrams
-- 
-- _Prim :: (Transformable a, Typeable a, Renderable a b) => Prism' (Prim b (V a)) a
-- _Prim = prism' Prim (\(Prim a) -> cast a)
-- 
-- 
