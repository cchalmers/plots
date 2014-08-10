{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -fno-warn-orphans       #-}
-- Wrapped Point
-- Typeable NullBackend
-- each for R2 and P2
-- lots of stuff for Recommend

-- | Bunch of random functions, some more usefull than others.


module Diagrams.Extra where

import Control.Lens          as L
import Data.Function         (on)
import Data.Monoid.Recommend
import Data.Typeable
import Diagrams.Core.Style
import Diagrams.Prelude      as D
import Diagrams.TwoD.Types
import Diagrams.ThreeD.Types as ThreeD
import Data.AffineSpace.Point

import qualified Data.Map as Map

-- diagrams misc

(##) :: AReview s t a b -> b -> t
(##) = review

infixr 8 ##

-- coordinates
class (HasX a, HasY a) => HasXY a where
  _xy :: Lens' a R2

instance HasXY R2 where
  _xy = id

instance HasXY R3 where
  _xy = lens (\(unr3 -> (x,y,_)) -> r2 (x, y))
             (\(unr3 -> (_,_,z)) (unr2 -> (x,y)) -> r3 (x,y,z))

-- Points

instance Rewrapped (Point v1) (Point v2)
instance Wrapped (Point b) where
  type Unwrapped (Point v) = v
  _Wrapped' = iso (\(P v) -> v) P


-- NullBackend

deriving instance Typeable NullBackend
deriving instance Typeable ThreeD.R3

-- Located -------------------------------------------------------------

-- | Lens onto the location of something located.
_loc :: Lens' (Located a) (Point (V a))
_loc = lens loc setter
  where setter (viewLoc -> (_, a)) p' = a `D.at` p'

-- | Transform the location of the trails of a path.
semanticTransform :: HasLinearMap v => Transformation v -> Path v -> Path v
semanticTransform t = over (_Wrapped . traversed . _loc) (papply t)

locate :: HasOrigin a => Located a -> a
locate (viewLoc -> (p,a)) = moveTo p a

-- SizeSpec ------------------------------------------------------------

-- | Isomorphism from 'SizeSpec2D' to @(Maybe width, Maybe height)@.
spec2D :: Iso' SizeSpec2D (Maybe Double, Maybe Double)
spec2D = iso getter (uncurry mkSizeSpec)
  where getter (Width w)  = (Just w, Nothing)
        getter (Height h) = (Nothing, Just h)
        getter (Dims w h) = (Just w, Just h)
        getter Absolute   = (Nothing, Nothing)

-- | Lens onto the possible width of a 'SizeSpec2D'.
specWidth :: Lens' SizeSpec2D (Maybe Double)
specWidth = spec2D . _1

-- | Lens onto the possible height of a 'SizeSpec2D'.
specHeight :: Lens' SizeSpec2D (Maybe Double)
specHeight = spec2D . _2

required2DScaling :: SizeSpec2D -> (Double, Double) -> T2
required2DScaling spec b
 | anyOf (L.beside each (traversed._Just)) (==0) (b, spec^.spec2D) = mempty
required2DScaling spec (w,h) = case spec of
  Absolute         -> mempty
  Width specW      -> scaling (specW / w)
  Height specH     -> scaling (specH / w)
  Dims specW specH -> scalingX (specW / w)
                   <> scalingY (specH / h)

-- Transforms ----------------------------------------------------------

-- | Construct a transformation which reflects along x=y.
reflectionXY :: T2
reflectionXY = fromLinear s s
  where s = (\(R2 x y) -> R2 y x) <-> (\(R2 x y) -> R2 y x)

reflectXY :: (V t ~ R2, Transformable t) => t -> t
reflectXY = D.transform reflectionXY

roundInt :: Double -> Int
roundInt = round

multipleOf :: Integral a => a -> a -> Bool
multipleOf 0 _ = True
multipleOf n x = x `mod` n == 0
{-# INLINE multipleOf #-}

-- frame without backend condition
frame' :: (HasLinearMap v, InnerSpace v, OrderedField (Scalar v), Monoid' m)
        => Scalar v -> QDiagram b v m -> QDiagram b v m
frame' s d = setEnvelope (onEnvelope t (d^.envelope)) d
  where
    t f x = f x + s

-- Coordinates ---------------------------------------------------------

instance Each P2 P2 Double Double where
  each = p2Iso . each

instance Each R2 R2 Double Double where
  each = r2Iso . each

instance Each P3 P3 Double Double where
  each = p3Iso . each

instance Each R3 R3 Double Double where
  each = r3Iso . each

-- Coordinates ---------------------------------------------------------

styleAttr :: forall a v. AttributeClass a => Lens' (Style v) (Maybe a)
styleAttr = lens getAttr setAttr'
  where
    setAttr' sty (Just a) = setAttr a sty
    setAttr' sty Nothing  = over _Wrapped (Map.delete (show . typeOf $ (undefined :: a))) sty

_Attribute :: AttributeClass a => Prism' (Attribute v) a
_Attribute = prism' mkAttr unwrapAttr

_TAttribute :: (AttributeClass a, Transformable a, V a ~ v)
            => Prism' (Attribute v) a
_TAttribute = prism' mkTAttr unwrapAttr

-- Situated ------------------------------------------------------------

newtype Situated a = Situated (Located a)

instance (t ~ Situated a') => Rewrapped (Situated a) t
instance Wrapped (Situated a) where
  type Unwrapped (Situated a) = Located a
  _Wrapped' = iso (\(Situated a) -> a) Situated
  {-# INLINE _Wrapped' #-}


-- | Lens onto the object, unadjusted.
inSitu :: Lens' (Situated a) a
inSitu = _Wrapped . located

situate :: HasOrigin a => Situated a -> a
situate (viewLoc . L.view _Wrapped -> (p,a)) = moveTo p a

type instance V (Situated a) = V a

deriving instance (Eq (V a), Eq a)     => Eq (Situated a)
deriving instance (Ord (V a), Ord a)   => Ord (Situated a)
deriving instance (Show (V a), Show a) => Show (Situated a)
deriving instance VectorSpace (V a)    => HasOrigin (Situated a)
deriving instance Enveloped a          => Enveloped (Situated a)
deriving instance Enveloped a          => Juxtaposable (Situated a)
deriving instance Traced a             => Traced (Situated a)
deriving instance Qualifiable a        => Qualifiable (Situated a)

-- | Only the location gets transformed.
instance HasLinearMap (V a) => Transformable (Situated a) where
  -- transform t (Situated l) = Situated (transformLoc t l)
  transform t = over (_Wrapped . _loc) (papply t)

-- Traverals

traverse2lens :: Lens' a b -> Lens' a b -> Traversal' a b
traverse2lens a b f x =
  (\x1 x2 -> x & a .~ x1
               & b .~ x2
  ) <$> f (x^.a) <*> f (x^.b)
{-# INLINE traverse2lens #-}

tupleLens :: Lens' a b1 -> Lens' a b2 -> Lens' a (b1,b2)
tupleLens a b f x =
  (\(x1,x2) -> x & a .~ x1
                 & b .~ x2
  ) <$> f (x^.a, x^.b)
{-# INLINE tupleLens #-}

-- Recommend

_Recommend :: Prism' (Recommend a) a
_Recommend = prism' Recommend getRec
  where
    getRec (Recommend a) = Just a
    getRec _             = Nothing

_Commit :: Prism' (Recommend a) a
_Commit = prism' Commit getCommit
  where
    getCommit (Commit a) = Just a
    getCommit _          = Nothing

recommend :: Lens' (Recommend a) a
recommend = lens getRecommend setRecommend
  where
    setRecommend (Recommend _) a = Recommend a
    setRecommend (Commit _   ) a = Commit a

instance Functor Recommend where
  fmap f (Recommend a) = Recommend (f a)
  fmap f (Commit a)    = Commit (f a)

fromCommit :: a -> Recommend a -> a
fromCommit _ (Commit a) = a
fromCommit a _          = a

-- do these even make sense?
-- instance Foldable Recommend where
--   foldr f b (Recommend a) = f a b
--   foldr f b (Commit a)    = f a b
--
--   foldl f b (Recommend a) = f b a
--   foldl f b (Commit a)    = f b a
--
-- instance Traversable Recommend where
--   traverse f (Recommend a) = Recommend <$> f a
--   traverse f (Commit a)    = Commit <$> f a

deriving instance (Show a) => Show (Recommend a)
deriving instance (Read a) => Read (Recommend a)
deriving instance (Eq a)   => Eq (Recommend a)
deriving instance (Ord a)  => Ord (Recommend a)

-- enumerations

enumFromToBy :: (Num a, Enum a) => a -> a -> a -> [a]
enumFromToBy a b x = [a, a + x .. b]

equating :: Eq b => (a -> b) -> a -> a -> Bool
equating = on (==)



