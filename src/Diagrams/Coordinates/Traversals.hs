{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}

{-# OPTIONS_GHC -fno-warn-orphans       #-}

-- | Traversable containers corresponding to diagrams coordinates.

module Diagrams.Coordinates.Traversals where

import           Control.Lens
import           Data.Typeable
import           Diagrams.Angle
import qualified Diagrams.Prelude        as D
import qualified Diagrams.Prelude.ThreeD as D
import qualified Linear                  as L
import Linear (E)

import Control.Applicative
import Control.Monad.Fix
import Control.Monad.Zip
import Data.Foldable
import Data.Functor.Bind
import GHC.Generics (Generic1)
import Data.Semigroup
import Data.Functor.Rep
import Data.Distributive
-- import Diagrams.Core.V
import Diagrams.Extra ()

-- | A class that provides a corresponding traversable containers for a 
--   diagarms coordinate.
-- class (Functor (T (V a)),
--        Applicative (T (V a)),
--        Traversable (T (V a)),
--        Foldable (T (V a)),
--        FunctorWithIndex (E (T (V a))) (T (V a)),
--        FoldableWithIndex (E (T (V a))) (T (V a)),
--        TraversableWithIndex (E (T (V a))) (T (V a)),
--        Each (T (V a) Double) (T (V a) Double) Double Double,
--        Distributive (T (V a)),
--        Representable (T (V a))
--        ) -- this is real ugly, but it saves uglyness later
--     => TraversableCoordinate a where
--   type T a :: * -> *
--   traversableCoord :: Iso' (V a) (T (V a) Double)
class (Functor (T v),
       Applicative (T v),
       Traversable (T v),
       Foldable (T v),
       FunctorWithIndex (E (T v)) (T v),
       FoldableWithIndex (E (T v)) (T v),
       TraversableWithIndex (E (T v)) (T v),
       Each (T v Double) (T v Double) Double Double,
       Distributive (T v),
       Representable (T v)
       )
    => TraversableCoordinate v where
  type T v :: * -> *
  traversableCoord :: Iso' v (T v Double)
-- class (Functor (LinearV v),
--        Applicative (LinearV v),
--        Traversable (LinearV v),
--        Foldable (LinearV v),
--        FunctorWithIndex (E (LinearV v)) (LinearV v),
--        FoldableWithIndex (E (LinearV v)) (LinearV v),
--        TraversableWithIndex (E (LinearV v)) (LinearV v),
--        Each (LinearV v Double) (LinearV v Double) Double Double,
--        Distributive (LinearV v),
--        Representable (LinearV v)
--        )
--     => TraversableCoordinate v where
--   type LinearV v :: * -> *
--   traversableCoord :: Iso' v (T v Double)
-- 
-- type T a = LinearV (V a)

diagramsCoord :: TraversableCoordinate v => Iso' (T v Double) v
diagramsCoord = from traversableCoord

instance TraversableCoordinate D.R2 where
  type T D.R2 = L.V2
  traversableCoord = iso (\(D.unr2 -> (x,y)) -> L.V2 x y)
                         (\(L.V2 x y)        -> x D.^& y)

instance TraversableCoordinate D.R3 where
  type T D.R3 = L.V3
  traversableCoord = iso (\(D.unr3 -> (x,y,z)) -> L.V3 x y z)
                         (\(L.V3 x y z)        -> x D.^& y D.^& z)


type Mat v = T v (T v Double)

-- instance TraversableCoordinate P.Polar where
--   type T P.Polar = PolarV
--   coordinateIso = iso (\(PolarV (L.V2 r θ))       -> (θ@@rad) D.^& r)
--                       (\(view _Wrapped' -> (θ,r)) -> PolarV (L.V2 r (θ^.rad)))

traversablePoint :: TraversableCoordinate v => Iso' (D.Point v) (T v Double)
traversablePoint = _Wrapped' . traversableCoord

-- classes

-- Radial: stuff with a radius.

-- | A space that has a radial basis vector _r.
class Radial t where
  _r :: Lens' (t a) a

-- | Radial basis element.
er :: Radial t => L.E t
er = L.E _r

-- | A space that has a radial component, not nessesary a basis vector.
class HasRadial t where
  type RadialResult t :: *
  r_ :: Lens' t (RadialResult t)

-- causes overlapping instances
-- instance Radial t => HasRadial (t a) where
--   type RadialResult (t a) = a
--   _r' = _r

instance HasRadial D.R2 where
  type RadialResult D.R2 = Double
  r_ = D._r


-- Polar: stuff with an angle

-- | A space that has a polar basis vector _θ.
class Polar t where
  _θ :: Lens' (t a) a

-- | Polar basis element
eθ :: Polar t => L.E t
eθ = L.E _θ

-- | A space that has a polar component, not nessesary a basis vector.
class HasPolar t where
  type PolarResult t :: *
  _θ' :: Lens' t (PolarResult t)

-- Azimuthal: stuff with second angle

-- | A space that has both a polar and azimuthal basis vector.
class Polar t => Azimuthal t where
  _φ :: Lens' (t a) a
  _θφ :: Lens' (t a) (Direction a)

-- | Azimuthal basis element.
eφ :: Azimuthal t => L.E t
eφ = L.E _θ

instance (Floating a) => HasRadial (L.V2 a) where
  type RadialResult (L.V2 a) = a
  r_ = lens L.norm (\v r -> r L.*^ L.signorm v)

-- polar type

newtype PolarV a = PolarV (L.V2 a)
  deriving (Monad, Functor, Typeable, MonadFix, Applicative, Traversable,
            Generic1, MonadZip, Foldable, Apply, Bind)

instance Rewrapped (PolarV a) (PolarV b)
instance Wrapped (PolarV a) where
  type Unwrapped (PolarV a) = L.V2 a
  _Wrapped' = iso (\(PolarV a) -> a) PolarV

instance HasRadial (PolarV a) where
  type RadialResult (PolarV a) = a
  r_ = _r

instance Radial PolarV where
  _r = _Wrapped . L._x

instance Polar PolarV where
  _θ = _Wrapped . L._y

mkPolar :: Double -> Angle -> PolarV Double
mkPolar r θ = PolarV $ L.V2 r (θ^.deg)

-- -- direction type
-- 
newtype Direction a = Direction (L.V2 a)
  deriving (Monad, Functor, Typeable, MonadFix, Applicative, Traversable,
            Generic1, MonadZip, Foldable, Apply, Bind, Semigroup)

instance Rewrapped (Direction a) (Direction b)
instance Wrapped (Direction a) where
  type Unwrapped (Direction a) = L.V2 a
  _Wrapped' = iso (\(Direction a) -> a) Direction

instance Polar Direction where
  _θ = _Wrapped . L._x

instance Azimuthal Direction where
  _φ  = _Wrapped . L._y
  _θφ = id






-- orphan semigroup instances

instance Semigroup a => Semigroup (L.V1 a) where
  (L.V1 x1) <> (L.V1 x2) = L.V1 (x1 <> x2)

instance Semigroup a => Semigroup (L.V2 a) where
  (L.V2 x1 y1) <> (L.V2 x2 y2) = L.V2 (x1 <> x2) (y1 <> y2)

instance Semigroup a => Semigroup (L.V3 a) where
  (L.V3 x1 y1 z1) <> (L.V3 x2 y2 z2) = L.V3 (x1 <> x2) (y1 <> y2) (z1 <> z2)

