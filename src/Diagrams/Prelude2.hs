{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

-- | Compatability layer between diagrams, lens and linear with lots of extras.

module Diagrams.Prelude2
  ( module Exports
    -- * Transformations with matricies.
  -- new classes
  , HasR1
  , HasR2
  , HasR3
  , HasR4
  -- generalised old functions
  , R2Backend
  , frame

  , unitX , unit_X
  , unitY , unit_Y
  , unitZ , unit_Z

  , scalingX
  , scalingY
  , scalingZ

  , reflectionX
  , reflectionY
  , reflectionZ
  
  ) where

import Control.Lens                    as Exports hiding (at, backwards,
                                                   none, transform,
                                                   ( # ), (.>), (<.>),
                                                   (|>), lmap)
import Data.Default as Exports
import Data.Distributive               as Exports
import Data.Foldable                   as Exports
import Data.Traversable                as Exports
import Diagrams.Coordinates            as Exports
import Diagrams.Coordinates.Traversals as Exports hiding (Direction, _r)
import Diagrams.Core                   as Exports
import Diagrams.Core.Transform         as Exports
import Diagrams.Core.Types             as Exports
import Diagrams.Extra                  as Exports
import Data.Typeable                   as Exports
import Diagrams.TwoD.Text              as Exports
import Diagrams.Prelude                as Exports hiding (beside, under,
                                                   view, unitX, unit_X, unitY, 
                                                   unit_Y, scalingX, scalingY,
                                                   reflectionX, reflectionY, 
                                                   frame)
import Diagrams.Prelude.ThreeD         as Exports (aboutX, aboutY, aboutZ)
import Diagrams.ThreeD.Types           as Exports hiding (spherical, _phi)
import Linear                          as Exports hiding (Conjugate, R1,
                                                   R2, R3, Trace, basis,
                                                   conjugate, distance,
                                                   lerp, rotate, sumV,
                                                   trace, translation,
                                                   (*^), (^*), (^+^),
                                                   (^-^), (^/), _x, _y,
                                                   _z, _xy)

-- import           Data.AdditiveGroup
import           Data.Basis
-- import           Data.LinearMap
-- import qualified Diagrams.Prelude        as D2
-- import qualified Diagrams.Prelude.ThreeD as D3
import qualified Linear                  as L

-- linear vector-spaces instances

instance Num a => AdditiveGroup (V2 a) where
  zeroV = zero
  (^+^) = (L.^+^)
  negateV = negate

instance Num a => VectorSpace (V2 a) where
  type Scalar (V2 a) = a
  (*^) = (L.*^)

instance (Num a, AdditiveGroup a) => InnerSpace (V2 a) where
  (<.>) = dot

instance Num a => HasBasis (V2 a) where
  type Basis (V2 a) = E V2
  basisValue = unit . el
  decompose (V2 x y) = [(ex, x), (ey, y)]
  decompose' v e = v ^. el e

instance Num a => AdditiveGroup (V3 a) where
  zeroV = zero
  (^+^) = (L.^+^)
  negateV = negate

instance Num a => VectorSpace (V3 a) where
  type Scalar (V3 a) = a
  (*^) = (L.*^)

instance (Num a, AdditiveGroup a) => InnerSpace (V3 a) where
  (<.>) = dot

instance Num a => HasBasis (V3 a) where
  type Basis (V3 a) = E V3
  basisValue = unit . el
  decompose (V3 x y z) = [(ex, x), (ey, y), (ez, z)]
  decompose' v e = v ^. el e

-- linear version of HasX but forall x
type HasR1 = L.R1
type HasR2 = L.R2
type HasR3 = L.R3
type HasR4 = L.R4

-- more general diagrams functions

type R2Backend b = (Renderable Text b, Renderable (Path R2) b, Backend b R2, Typeable b)

spherical :: Iso' R3 (Double, Angle, Angle)
spherical = iso
  (\v@(unr3 -> (x,y,z)) -> (magnitude v, atan2A y x, acosA (z / magnitude v)))
  (\(r,θ,φ) -> r3 (r * cosA θ * sinA φ, r* sinA θ * sinA φ, r * cosA φ))

_theta :: Lens' R3 Angle
_theta = spherical . _2

_phi :: Lens' R3 Angle
_phi = spherical . _3


-- frame without backend dependency
frame :: (HasLinearMap v, InnerSpace v, OrderedField (Scalar v), Monoid' m)
        => Scalar v -> QDiagram b v m -> QDiagram b v m
frame s d = setEnvelope (onEnvelope t (d ^. envelope)) d
  where
    t f x = f x + s

unitX :: (HasX v, VectorSpace v) => v
unitX = zeroV & _x .~ 1

unit_X :: (HasX v, VectorSpace v) => v
unit_X = zeroV & _x .~ (-1)

unitY :: (HasY v, VectorSpace v) => v
unitY = zeroV & _y .~ 1

unit_Y :: (HasY v, VectorSpace v) => v
unit_Y = zeroV & _y .~ (-1)

unitZ :: (HasZ v, VectorSpace v) => v
unitZ = zeroV & _z .~ 1

unit_Z :: (HasZ v, VectorSpace v) => v
unit_Z = zeroV & _z .~ (-1)

scalingX :: (Scalar v ~ Double, HasX v, HasLinearMap v) => Double -> Transformation v
scalingX s = fromLinear f f
  where f = (_x *~ s) <-> (_x //~ s)

scalingY :: (Scalar v ~ Double, HasY v, HasLinearMap v) => Double -> Transformation v
scalingY s = fromLinear f f
  where f = (_y *~ s) <-> (_y //~ s)

scalingZ :: (Scalar v ~ Double, HasZ v, HasLinearMap v) => Double -> Transformation v
scalingZ s = fromLinear f f
  where f = (_z *~ s) <-> (_z //~ s)

reflectionX :: (Scalar v ~ Double, HasX v, HasLinearMap v) => Transformation v
reflectionX = scalingX (-1)

reflectionY :: (Scalar v ~ Double, HasY v, HasLinearMap v) => Transformation v
reflectionY = scalingY (-1)

reflectionZ :: (Scalar v ~ Double, HasZ v, HasLinearMap v) => Transformation v
reflectionZ = scalingZ (-1)


-- fromMat22 :: M22 Double -> Transformation R2
-- fromMat22 m = Transformation (invertable22 m) (invertable22 $ distribute m) zeroV
-- 
-- invertable22 :: M22 Double -> R2 :-: R2
-- invertable22 m = asLinearMap22 m <-> maybe id asLinearMap22 (inv22 m)
-- 
-- asLinearMap22 :: M22 Double -> R2 -> R2
-- asLinearMap22 m = over traversableCoord (m !*)
-- 
-- fromMat33 :: M33 Double -> Transformation R3
-- fromMat33 m = Transformation (invertable33 m) (invertable33 $ distribute m) zeroV
-- 
-- invertable33 :: M33 Double -> R3 :-: R3
-- invertable33 m = asLinearMap33 m <-> maybe id asLinearMap33 (inv33 m)
-- 
-- asLinearMap33 :: M33 Double -> R3 -> R3
-- asLinearMap33 m = over traversableCoord (m !*)
-- 
-- toM22 :: (R2 :-* R2) -> M22 Double
-- toM22 l = V2 (V2 a b) (V2 c d)
--   where
--     (a,b) = unr2 $ lapply l D2.unitX
--     (c,d) = unr2 $ lapply l D2.unitY
-- 
-- toM33 :: (R3 :-* R3) -> M33 Double
-- toM33 l = V3 (V3 a b c) (V3 d e f) (V3 g h i)
--   where
--     (a,b,c) = unr3 $ lapply l D3.unitX
--     (d,e,f) = unr3 $ lapply l D3.unitY
--     (g,h,i) = unr3 $ lapply l D3.unitZ
 

-- fromTransformationR2 :: Transformation R2 -> M22 Double
-- fromTransformationR2 t = V2 (V2 a b) (V2 c d)
--   where
--     (a,b) = unr2 $ apply t D2.unitX
--     (c,d) = unr2 $ apply t D2.unitY
--     -- (I don't like pattern matching lists from things like @onBasis@)

-- toMat33 :: Transformation R3 -> M33 Double
-- toMat33 t = V3 (V3 a b c) (V3 d e f) (V3 g h i)
--   where
--     (a,b,c) = unr3 $ apply t D3.unitX
--     (d,e,f) = unr3 $ apply t D3.unitY
--     (g,h,i) = unr3 $ apply t D3.unitZ

-- instance Num a => AdditiveGroup (V2 a) where
--   zeroV = zero
--   (^+^) = (L.^+^)
--   negateV = negate

