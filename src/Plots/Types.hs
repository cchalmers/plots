{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE OverlappingInstances             #-}

{-# LANGUAGE CPP             #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Plots.Types
  (
  -- * Bounds
    Bound (..)
  , Bounds
  , HasBounds (..)
  , getBounds
  , getBound
  , upperBound
  , lowerBound
  -- , xMax, xMin -- , xAxisBounds
  -- , yMin, yMax -- , yAxisBounds
  -- , zMin, zMax -- , zAxisBounds
  , boundsMin, boundsMax
  -- , zMin, zMax, zAxisBounds

  -- * Orientation
  , Orientation (..)
  , orient

  -- * Legend
  , LegendEntry
  , legendPic
  , legendText
  , legendPrecidence

  -- * Generic plot
  , GenericPlot
  , HasGenericPlot (..)

  -- ** Themes
  , commitTheme
  , commitCurrentTheme

  -- * Plot / Plotable
  , Plotable (..)
  , Plot (..)
  , _Plot
  -- , plotLineStyle
  -- , plotMarkerStyle
  -- , plotFillStyle
  ) where

import Control.Lens     as L hiding (transform, ( # ), (|>))
import Data.Default
import Data.Typeable
import Diagrams.Prelude as D hiding (view)
import Diagrams.Extra
import Data.Functor.Rep
import Diagrams.ThreeD
import Data.Monoid.Recommend
import Diagrams.BoundingBox

import Plots.Themes

import Diagrams.Coordinates.Traversals
import qualified Linear as L
import Linear (E, el)

import Data.LinearMap

-- Bounds

-- type Bounds = (Double, Double)
data Bound = Bound
  { _lowerBound :: Recommend Double
  , _upperBound :: Recommend Double
  }

makeLenses ''Bound

type Bounds v = T v Bound

instance Default Bound where
  def = Bound (Recommend 0) (Recommend 5)

-- instance Semigroup Bound where
--   Bound l1 u1 <> Bound l2 u2 = Bound (min l1 l2) (max u1 u2)

class HasBounds a v | a -> v where
  bounds :: Lens' a (T v Bound)
-- #ifdef HLINT
--   default bounds :: (a ~ T v Bound) => Lens' a a
--   bounds = id
-- #endif

instance HasBounds (L.V2 Bound) R2 where
  bounds = id
  {-# INLINE bounds #-}

instance HasBounds (L.V3 Bound) R3 where
  bounds = id
  {-# INLINE bounds #-}

getBounds :: Bound -> (Double,Double)
getBounds (Bound l u) = (getRecommend l, getRecommend u)

getBound :: HasBounds a v => E (T v) -> a -> (Double, Double)
getBound e a = getBounds $ a ^. bounds . el e



--   bounds = id

-- type Domain v = B v Bound
-- class HasDomain a t where
--   domain :: Lens' a (t Bounds)
-- 
-- instance HasDomain (t Bounds) t where
--   domain = id

-- seems over engineered, only way I can get it to work
-- class HasDomain a where
--   type DomainType a :: * -> *
--   domain :: Lens' a (DomainType a Bound)

-- instance HasDomain (t Bound) where
--   type DomainType (t Bound) = t
--   domain = id


-- instance HasDomain (L.V2 Bounds) L.V2 where
--   domain = id


boundsMin :: (T v ~ t, Representable t, HasBounds a v) => Lens' a (t (Recommend Double))
boundsMin = bounds . L.column lowerBound

boundsMax :: (T v ~ t, TraversableCoordinate v, HasBounds a v) => Lens' a (Point v)
boundsMax = bounds . L.column (upperBound . recommend) . diagramsCoord . _Unwrapped

-- xMin :: (T v ~ t, L.R1 t, HasBounds a v) => Lens' a (Recommend Double)
-- xMin = bounds . L._x . lowerBound
-- 
-- xMax :: (T v ~ t, L.R1 t, HasBounds a v) => Lens' a Double
-- xMax = bounds . L._x . upperBound
-- 
-- yMin :: (T v ~ t, L.R2 t, HasBounds a v) => Lens' a Double
-- yMin = bounds . L._y . lowerBound
-- 
-- yMax :: (T v ~ t, L.R2 t, HasBounds a v) => Lens' a Double
-- yMax = bounds . L._y . upperBound
-- 
-- zMin :: (T v ~ t, L.R3 t, HasBounds a v) => Lens' a Double
-- zMin = bounds . L._z . lowerBound
-- 
-- zMax :: (T v ~ t, L.R3 t, HasBounds a v) => Lens' a Double
-- zMax = bounds . L._z . upperBound


-- data Limit = Limit
--   { _lowerLimit :: Double
--   , _upperLimit :: Double
--   } deriving (Typeable, Show)
-- 
-- makeLenses ''Limit
-- 
-- type Limits v = T v (Maybe Limit)
-- 
-- instance Semigroup Limit where
--   Limit l1 u1 <> Limit l2 u2 = Limit (min l1 l2) (max u1 u2)
-- 
-- class HasLimits a v | a -> v where
--   limits :: Lens' a (Limits v)
-- #ifdef HLINT
--   default limits :: (a ~ Limits v) => Lens' a a
--   limits = id
-- #endif
-- 
-- instance HasLimits (L.V2 (Maybe Limit)) R2
-- instance HasLimits (L.V3 (Maybe Limit)) R3
-- 
-- getLimits :: Limit -> (Double,Double)
-- getLimits (Limit l u) = (l, u)
-- 
-- getLimit :: HasLimits a v => E (T v) -> a -> Maybe (Double, Double)
-- getLimit e a = getLimits <$> a ^. limits . el e



-- data AxisBounds v = AxisBounds
--   { _minPoint :: Point v
--   , _maxPoint :: Point v
--   } deriving (Typeable, Show)
-- 
-- makeClassy ''AxisBounds
-- 
-- instance Default (AxisBounds R2) where
--   def = AxisBounds
--           { _minPoint = origin
--           , _maxPoint = 5 ^& 5
--           }


-- make a Plots.Simple module with lots of functions like this? Right now it's 
-- cluttering the important stuff

-- xMin :: (HasAxisBounds a v, HasX (Point v)) => Lens' a Double
-- xMin = minPoint . _x
-- 
-- xMax :: (HasAxisBounds a v, HasX (Point v)) => Lens' a Double
-- xMax = maxPoint . _x
-- 
-- yMin :: (HasAxisBounds a v, HasY (Point v)) => Lens' a Double
-- yMin = minPoint . _y
-- 
-- yMax :: (HasAxisBounds a v, HasY (Point v)) => Lens' a Double
-- yMax = maxPoint . _y
-- 
-- zMin :: (HasAxisBounds a v, HasZ (Point v)) => Lens' a Double
-- zMin = minPoint . _z
-- 
-- zMax :: (HasAxisBounds a v, HasZ (Point v)) => Lens' a Double
-- zMax = maxPoint . _z

-- messy definition, I can't think of a cleaner way.
-- instance (Each (Point v) (Point v) a a, Ord a) => Semigroup (AxisBounds v) where
--   AxisBounds minA maxA <> AxisBounds minB maxB
--     = AxisBounds (minA & partsOf each %~ zipWith min (minB ^.. each))
--                  (maxA & partsOf each %~ zipWith max (maxB ^.. each))


-- -- I don't like this
-- data AxisBounds = AxisBounds
--   { _xMin :: Double
--   , _xMax :: Double
--   , _yMin :: Double
--   , _yMax :: Double
--   } deriving Show
-- 
-- makeClassy ''AxisBounds
-- 
-- instance Default AxisBounds where
--   def = AxisBounds 0 5 0 5
-- 
-- instance Semigroup AxisBounds where
--   (AxisBounds xM1 xm1 yM1 ym1) <> (AxisBounds xM2 xm2 yM2 ym2)
--     = AxisBounds (max xM1 xM2) (min xm1 xm2) (max yM1 yM2) (min ym1 ym2)

-- -- | Lens onto x bounds tuple.
-- -- xAxisBounds :: HasAxisBounds a => Lens' a (Double, Double)
-- xAxisBounds :: (HasAxisBounds a v, HasX (Point v)) => Lens' a (Double, Double)
-- xAxisBounds = tupleLens xMin xMax
-- {-# INLINE xAxisBounds #-}
-- 
-- -- | Lens onto y bounds tuple.
-- -- yAxisBounds :: HasAxisBounds a => Lens' a (Double, Double)
-- yAxisBounds :: (HasAxisBounds a v, HasY (Point v)) => Lens' a (Double, Double)
-- yAxisBounds = tupleLens yMin yMax
-- {-# INLINE yAxisBounds #-}
-- 
-- -- | Lens onto y bounds tuple.
-- -- yAxisBounds :: HasAxisBounds a => Lens' a (Double, Double)
-- zAxisBounds :: (HasAxisBounds a v, HasZ (Point v)) => Lens' a (Double, Double)
-- zAxisBounds = tupleLens zMin zMax
-- {-# INLINE zAxisBounds #-}

-- Bounding box stuff

-- -- | Only a valid isomorphism if point set is valid (otherwise it returns something).
-- boundingBoxL :: (HasLinearMap v, OrderedField (Scalar v), Ord (Basis v))
--   => Iso' (BoundingBox v) (Maybe (Point v, Point v))
-- boundingBoxL = iso getCorners setCorners
--   where
--     setCorners (Just (l, u)) = fromCorners u l
--     setCorners Nothing       = emptyBox

-- getBoundingBasis :: TraversableCoordinate v => E (T v) -> BoundingBox v -> Maybe (Double, Double)
-- getBoundingBasis e b = over both (^. traversablePoint . el e) <$> getCorners b
                   

-- Orientation

data Orientation = Horizontal | Verticle

-- Take two functions for each outcome for an orientation.
orient :: Orientation -> a -> a -> a
orient Horizontal h _ = h
orient Verticle   _ v = v

-- Legends

data LegendEntry b = LegendEntry
  { _legendPic        :: ThemeEntry b -> Diagram b R2
  , _legendText       :: String
  , _legendPrecidence :: Double
  } deriving Typeable

makeLenses ''LegendEntry

instance Default (LegendEntry b) where
  def = LegendEntry mempty "" 0

-- Generic Plot info

-- | Data type for holding information all plots must contain.
data GenericPlot b v = GenericPlot
  { _plotTransform   :: Transformation v
  , _plotBounds      :: Bounds v
  , _clipPlot        :: Bool
  , _plotThemeEntry  :: Recommend (ThemeEntry b)
  , _legendEntries   :: [LegendEntry b]
  , _plotName        :: Name
  , _plotBoundingBox :: BoundingBox v
  } deriving Typeable

-- makeLensesWith (classyRules & generateSignatures .~ False) ''GenericPlot
-- makeClassy ''GenericPlot

-- I don't know how to give documenation for classes using TH, so I just wrote 
-- it by hand.

-- NOTE: I think I can get rid of the @v@ if I replace it with @V a@.

-- | Class that gives a lens onto a 'genericPlot'. All 'Plot's must impliment 
--   this class.
class HasGenericPlot t b v | t -> b, t -> v where

  {-# MINIMAL genericPlot #-}
  genericPlot :: Lens' t (GenericPlot b v)

  -- | Clip anything outside the current axis bounds.
  clipPlot :: Lens' t Bool
  clipPlot = genericPlot . lens
    (\GenericPlot { _clipPlot = a } -> a)
    (\g a -> g { _clipPlot = a})
  {-# INLINE clipPlot #-}

  -- | The theme entry to be used for the current plot.
  plotThemeEntry :: Lens' t (Recommend (ThemeEntry b))
  plotThemeEntry = genericPlot . lens
    (\GenericPlot { _plotThemeEntry = a } -> a)
    (\g a -> g { _plotThemeEntry = a})
  {-# INLINE plotThemeEntry #-}

  -- | The legend entries to be used for the current plot.
  legendEntries :: Lens' t [LegendEntry b]
  legendEntries = genericPlot . lens
    (\GenericPlot { _legendEntries = a } -> a)
    (\g a -> g { _legendEntries = a})
  {-# INLINE legendEntries #-}

  -- | The bounds the current plot requests.
  plotBounds :: Lens' t (Bounds v)
  plotBounds = genericPlot . lens
    (\GenericPlot { _plotBounds = a } -> a)
    (\g a -> g { _plotBounds = a})
  {-# INLINE plotBounds #-}

  -- | The name of the plot. This name is given to the rendered diagram.
  plotName :: Lens' t Name
  plotName = genericPlot . lens
    (\GenericPlot { _plotName = a } -> a)
    (\g a -> g { _plotName = a})
  {-# INLINE plotName #-}

  -- | The transformation to be applied to the plot.
  plotTransform :: Lens' t (Transformation v)
  plotTransform = genericPlot . lens
    (\GenericPlot { _plotTransform = a } -> a)
    (\g a -> g { _plotTransform = a})
  {-# INLINE plotTransform #-}

  -- | The transformation to be applied to the plot.
  plotBoundingBox :: Lens' t (BoundingBox v)
  plotBoundingBox = genericPlot . lens
    (\GenericPlot { _plotBoundingBox = a } -> a)
    (\g a -> g { _plotBoundingBox = a})
  {-# INLINE plotBoundingBox #-}

instance HasGenericPlot (GenericPlot b v) b v where
  genericPlot = id
  {-# INLINE genericPlot #-}

-- Some orphan overlapping instances. Should be alright as long as these 
-- instances arn't defined for anything with HasGenericPlot elsewhere. The 
-- alternative is to have this all these instances defined for each plot or 
-- rewrite lenses specific to HasGenericPlot.

instance HasGenericPlot a b v => HasThemeEntry a b where
  themeEntry = plotThemeEntry . recommend

-- | The style is applied to all theme styles. Only works for R2 due to 
--   HasStyle limitations.
instance (HasGenericPlot a b v, V a ~ R2) => HasStyle a where
  applyStyle sty = over themeEntry
                 $ over themeLineStyle (applyStyle sty)
                 . over themeMarkerStyle (applyStyle sty)
                 . over themeFillStyle (applyStyle sty)

instance (HasGenericPlot a b (V a), HasLinearMap (V a)) => Transformable a where
  transform = over plotTransform . transform

instance HasGenericPlot a b v => HasBounds a v where
  bounds = plotBounds

-- | Move origin by applying to @plotTransform@.
instance (HasGenericPlot a b (V a), HasLinearMap (V a)) => HasOrigin a where
  moveOriginTo = over plotTransform . moveOriginTo

instance HasGenericPlot a b v => Qualifiable a where
  n |> p = over plotName (n |>) p

-- | The @themeEntry@ lens goes though recommend, so @set themeEntry myTheme 
--   myPlot@ won't give a committed theme entry (so theme from axis will 
--   override). Use commitTheme to make sure theme is committed.
commitTheme :: HasGenericPlot a b v => ThemeEntry b -> a -> a
commitTheme = set plotThemeEntry . Commit

-- | Make the current theme a committed theme. See @commitTheme@.
commitCurrentTheme :: HasGenericPlot a b v => a -> a
commitCurrentTheme = over plotThemeEntry makeCommitted
  where
    makeCommitted (Recommend a) = Commit a
    makeCommitted c             = c
    

type instance V (GenericPlot b v) = v

instance (Renderable (Path R2) b, HasLinearMap v, Applicative (T v))
    => Default (GenericPlot b v) where
  def = GenericPlot
          { _plotTransform   = mempty
          , _plotBounds      = pure def
          , _clipPlot        = True
          , _plotThemeEntry  = Recommend def
          , _legendEntries   = []
          , _plotName        = mempty
          , _plotBoundingBox = emptyBox
          }

instance Renderable (Path R2) b => Default (GenericPlot b R3) where
  def = GenericPlot
          { _plotTransform   = mempty
          , _plotBounds      = pure def
          , _clipPlot        = True
          , _plotThemeEntry  = Recommend def
          , _legendEntries   = []
          , _plotName        = mempty
          , _plotBoundingBox = emptyBox
          }

-- -- | The style is applied to all theme styles. Only works for R2 due to 
-- --   HasStyle limitations.
-- instance HasStyle (GenericPlot b R2) where
--   applyStyle sty = over themeEntry
--                  $ over themeLineStyle (applyStyle sty)
--                  . over themeMarkerStyle (applyStyle sty)
--                  . over themeFillStyle (applyStyle sty)
-- 
-- instance HasLinearMap v => Transformable (GenericPlot b v) where
--   transform = over plotTransform . transform
-- 
-- instance HasBounds (GenericPlot b v) v where
--   bounds = plotBounds
-- 
-- -- | Move origin by applying to @plotTransform@.
-- instance HasLinearMap v => HasOrigin (GenericPlot b v) where
--   moveOriginTo = over plotTransform . moveOriginTo
-- 
-- instance Qualifiable (GenericPlot b v) where
--   n |> p = over plotName (n |>) p
   

-- Plot data type

-- | General class for something that can be wrapped in 'Plot'. The 'plot' 
--   function is rarely used by the end user.
class (HasGenericPlot a b v, Typeable a, Typeable b) => Plotable a b v where
  plot :: T v (Double, Double) -> Transformation v -> (v :-* R2) -> T2 -> a -> Diagram b R2

-- | Existential wrapper for something plotable.
data Plot b v = forall a. Plotable a b v => Plot a
  deriving Typeable

type instance V (Plot b v) = v

instance (Typeable b, Typeable v) => Plotable (Plot b v) b v where
  plot bs tv l t2 (Plot p) = plot bs tv l t2 p

instance HasGenericPlot (Plot b v) b v where
  genericPlot = lens (\(Plot a)    -> view genericPlot a)
                     (\(Plot a) gp -> Plot (set genericPlot gp a))

-- instance HasStyle (Plot b R2) where
--   applyStyle sty = over genericPlot (applyStyle sty)
-- 
-- instance HasLinearMap v => Transformable (Plot b v) where
--   transform = over genericPlot . transform
-- 
-- instance HasLinearMap v => HasOrigin (Plot b v) where
--   moveOriginTo = over genericPlot . moveOriginTo
-- 
-- instance Qualifiable (Plot b v) where
--   n |> p = over genericPlot (n |>) p
-- 
-- instance HasBounds (Plot b v) v where
--   bounds = genericPlot . bounds

-- | Prism onto the unwrapped plotable type. All standard plots export a 
--   specialised version of this which is normally more usefull (i.e. 
--   '_LinePlot').
_Plot :: Plotable a b v => Prism' (Plot b v) a
_Plot = prism' Plot (\(Plot a) -> cast a)

-- -- needed to prevent overlapping instances
-- plotLineStyle :: HasGenericPlot a b v => Traversal' a (Style R2)
-- plotLineStyle = genericPlot . themeEntry . _Commit . themeLineStyle
-- 
-- plotFillStyle :: HasGenericPlot a b v => Traversal' a (Style R2)
-- plotFillStyle = genericPlot . themeEntry . _Commit . themeFillStyle
-- 
-- plotMarkerStyle :: HasGenericPlot a b v => Traversal' a (Style R2)
-- plotMarkerStyle = genericPlot . themeEntry . _Commit . themeFillStyle
-- 
