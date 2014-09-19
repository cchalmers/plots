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
import Diagrams.Prelude      as D


-- diagrams misc

(##) :: AReview s t a b -> b -> t
(##) = review

infixr 8 ##

-- NullBackend

-- Located -------------------------------------------------------------

-- | Lens onto the location of something located.
_loc :: Lens' (Located a) (Point (V a) (N a))
_loc = lens loc setter
  where setter (viewLoc -> (_, a)) p' = a `D.at` p'

-- | Transform the location of the trails of a path.
semanticTransform :: (Num n, HasLinearMap v) => Transformation v n -> Path v n -> Path v n
semanticTransform t = over (_Wrapped . traversed . _loc) (papply t)

locate :: (HasOrigin a, Additive (V a), Num (N a)) => Located a -> a
locate (viewLoc -> (p,a)) = moveTo p a

-- SizeSpec ------------------------------------------------------------

-- | Isomorphism from 'SizeSpec2D' to @(Maybe width, Maybe height)@.
spec2D :: Iso' (SizeSpec2D n) (Maybe n, Maybe n)
spec2D = iso getter (uncurry mkSizeSpec)
  where getter (Width w)  = (Just w, Nothing)
        getter (Height h) = (Nothing, Just h)
        getter (Dims w h) = (Just w, Just h)
        getter Absolute   = (Nothing, Nothing)

-- | Lens onto the possible width of a 'SizeSpec2D'.
specWidth :: Lens' (SizeSpec2D n) (Maybe n)
specWidth = spec2D . _1

-- | Lens onto the possible height of a 'SizeSpec2D'.
specHeight :: Lens' (SizeSpec2D n) (Maybe n)
specHeight = spec2D . _2

required2DScaling :: (Floating n, Eq n) => SizeSpec2D n -> (n, n) -> T2 n
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
reflectionXY :: Num n => T2 n
reflectionXY = fromLinear s s
  where s = (\(V2 x y) -> V2 y x) <-> (\(V2 x y) -> V2 y x)

reflectXY :: (V t ~ V2, Transformable t, Num (N t)) => t -> t
reflectXY = D.transform reflectionXY

roundInt :: Double -> Int
roundInt = round

multipleOf :: Integral a => a -> a -> Bool
multipleOf 0 _ = True
multipleOf n x = x `mod` n == 0
{-# INLINE multipleOf #-}

-- Coordinates ---------------------------------------------------------

-- styleAttr :: forall a v. AttributeClass a => Lens' (Style v) (Maybe a)
-- styleAttr = lens getAttr setAttr'
--   where
--     setAttr' sty (Just a) = setAttr a sty
--     setAttr' sty Nothing  = over _Wrapped (Map.delete (show . typeOf $ (undefined :: a))) sty
-- 
-- _Attribute :: AttributeClass a => Prism' (Attribute v) a
-- _Attribute = prism' mkAttr unwrapAttr
-- 
-- _TAttribute :: (AttributeClass a, Transformable a, V a ~ v)
--             => Prism' (Attribute v) a
-- _TAttribute = prism' mkTAttr unwrapAttr

-- Situated ------------------------------------------------------------

-- newtype Situated a = Situated (Located a)
-- 
-- instance (t ~ Situated a') => Rewrapped (Situated a) t
-- instance Wrapped (Situated a) where
--   type Unwrapped (Situated a) = Located a
--   _Wrapped' = iso (\(Situated a) -> a) Situated
--   {-# INLINE _Wrapped' #-}
-- 
-- 
-- -- | Lens onto the object, unadjusted.
-- inSitu :: Lens' (Situated a) a
-- inSitu = _Wrapped . located
-- 
-- situate :: HasOrigin a => Situated a -> a
-- situate (viewLoc . L.view _Wrapped -> (p,a)) = moveTo p a
-- 
-- type instance V (Situated a) = V a
-- 
-- deriving instance (Eq (V a), Eq a)     => Eq (Situated a)
-- deriving instance (Ord (V a), Ord a)   => Ord (Situated a)
-- deriving instance (Show (V a), Show a) => Show (Situated a)
-- deriving instance Additive (V a)       => HasOrigin (Situated a)
-- deriving instance Enveloped a          => Enveloped (Situated a)
-- deriving instance Enveloped a          => Juxtaposable (Situated a)
-- deriving instance Traced a             => Traced (Situated a)
-- deriving instance Qualifiable a        => Qualifiable (Situated a)
-- 
-- -- | Only the location gets transformed.
-- instance HasLinearMap (V a) => Transformable (Situated a) where
--   -- transform t (Situated l) = Situated (transformLoc t l)
--   transform t = over (_Wrapped . _loc) (papply t)
-- 
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


pathFromVertices :: (Metric v, OrderedField n) => [Point v n] -> Path v n
pathFromVertices = fromVertices

