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

boundsMin :: (T v ~ t, Representable t, HasBounds a v) => Lens' a (t (Recommend Double))
boundsMin = bounds . L.column lowerBound

boundsMax :: (T v ~ t, TraversableCoordinate v, HasBounds a v) => Lens' a (Point v)
boundsMax = bounds . L.column (upperBound . recommend) . diagramsCoord . _Unwrapped
     

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


-- | Prism onto the unwrapped plotable type. All standard plots export a 
--   specialised version of this which is normally more usefull (i.e. 
--   '_LinePlot').
_Plot :: Plotable a b v => Prism' (Plot b v) a
_Plot = prism' Plot (\(Plot a) -> cast a)

