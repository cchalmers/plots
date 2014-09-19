
module Diagrams.Projections where

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Projections
-- Copyright   :  (c) 2014
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  c.chalmers@me.com
--
-- Projections are a way of viewing a three-dimensional objects on a 
-- two-dimensional plane.
-- 
-- (Note: information is from Wikipedia and guaranteed to be accurate.)
--
-----------------------------------------------------------------------------

import Diagrams.Prelude hiding (view)
import Control.Lens hiding (transform)
import Diagrams.ThreeD

-- * Parallel projections

-- ** Orthographic projections
-- Orthographic projections are a form of parallel projections where are 
-- projection lines are orthogonal to the projection plane.
--
-- For R3 this is achieved by ignoring one of the coordinates. For generality 
-- these are supplied as lenses.

-- | Orthographic projection onto the x-plane.
yz_ :: Lens' (V3 n) (V2 n)
yz_ = lens (\(V3 _ y z)          -> V2 y z)
           (\(V3 x _ _) (V2 y z) -> V3 x y z)
{-# INLINE yz_ #-}

-- | Orthographic projection onto the z-plane.
xy_ :: Lens' (V3 n) (V2 n)
xy_ = lens (\(V3 x y _)          -> V2 x y)
           (\(V3 _ _ z) (V2 x y) -> V3 x y z)
{-# INLINE xy_ #-}

-- | Orthographic projection onto the y-plane.
xz_ :: Lens' (V3 n) (V2 n)
xz_ = lens (\(V3 x _ z)          -> V2 x z)
           (\(V3 _ y _) (V2 x z) -> V3 x y z)
{-# INLINE xz_ #-}

-- ** Axonometric projection
-- Axonometric projections are a type of orthographic projection where the 
-- plane of the object is not parallel to the projection plane. This is the 
-- most common projection for technical drawings and plots.

-- *** Common axonometric projections
   
isometricProjection :: Floating n => V3 n -> V2 n
isometricProjection v = view yz_ v'
  where
    v' = transform t v
    t  = aboutY (45@@deg) <> aboutZ (asinA (tan (pi/6)))

