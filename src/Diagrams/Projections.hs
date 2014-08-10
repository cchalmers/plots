{-# LANGUAGE ViewPatterns #-}

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
import qualified Diagrams.Prelude.ThreeD as ThreeD
import Diagrams.ThreeD.Types
import Control.Lens hiding (transform)

-- * Parallel projections

-- ** Orthographic projections
-- Orthographic projections are a form of parallel projections where are 
-- projection lines are orthogonal to the projection plane.
--
-- For R3 this is achieved by ignoring one of the coordinates. For generality 
-- these are supplied as lenses.

-- | Orthographic projection onto the x-plane.
yz_ :: Lens' R3 R2
yz_ = lens (\(unr3 -> (_,y,z)) -> r2 (y,z))
           (\(unr3 -> (x,_,_)) (unr2 -> (y,z)) -> r3 (x,y,z))

-- | Orthographic projection onto the z-plane.
xy_ :: Lens' R3 R2
xy_ = lens (\(unr3 -> (x,y,_)) -> r2 (x,y))
           (\(unr3 -> (_,_,z)) (unr2 -> (x,y)) -> r3 (x,y,z))

-- | Orthographic projection onto the y-plane.
xz_ :: Lens' R3 R2
xz_ = lens (\(unr3 -> (x,_,z)) -> r2 (x,z))
           (\(unr3 -> (_,y,_)) (unr2 -> (x,z)) -> r3 (x,y,z))

-- ** Axonometric projection
-- Axonometric projections are a type of orthographic projection where the 
-- plane of the object is not parallel to the projection plane. This is the 
-- most common projection for technical drawings and plots.

-- *** Common axonometric projections
   
isometricProjection :: R3 -> R2
isometricProjection v = view yz_ v'
  where
    v' = transform t v
    t  = ThreeD.aboutY (45@@deg) <> ThreeD.aboutZ (asinA (tan (pi/6)))

