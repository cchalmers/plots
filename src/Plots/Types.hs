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
{-# LANGUAGE OverlappingInstances      #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE GADTs      #-}

{-# LANGUAGE CPP                        #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Plots.Types
  (
  -- * Bounds
    Bound (..)
  , Bounds (..)
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
  , LegendPic (..)
  , legendPic
  , legendText
  , legendPrecidence
  , mkLegendEntry

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
  , B
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
import Data.Monoid.Recommend
import Diagrams.BoundingBox

import Plots.Themes
import Linear

type family B a :: *

-- Bounds

-- | A 'Bound' allows you to 'Commit' a an upper or lower bound while having a 
--   fallback 'Reccommend' value.
data Bound n = Bound
  { _lowerBound :: Recommend n
  , _upperBound :: Recommend n
  }
  deriving Show

makeLenses ''Bound

newtype Bounds v n = Bounds (v (Bound n))

makeWrapped ''Bounds

type instance V (Bounds v n) = v
type instance N (Bounds v n) = n

instance Num n => Default (Bound n) where
  def = Bound (Recommend 0) (Recommend 5)

liftRecommend :: (a -> a -> a) -> Recommend a -> Recommend a -> Recommend a
liftRecommend _ (Commit a) (Recommend _)    = Commit a
liftRecommend _ (Recommend _) (Commit b)    = Commit b
liftRecommend f (Recommend a) (Recommend b) = Recommend (f a b)
liftRecommend f (Commit a) (Commit b)       = Commit (f a b)

instance Ord n => Semigroup (Bound n) where
  Bound l1 u1 <> Bound l2 u2 = Bound (liftRecommend min l1 l2) (liftRecommend max u1 u2)

class HasBounds a where
  bounds :: Lens' a (Bounds (V a) (N a))

instance HasBounds (Bounds V2 n) where
  bounds = id
  {-# INLINE bounds #-}

instance HasBounds (Bounds V3 n) where
  bounds = id
  {-# INLINE bounds #-}

getBounds :: Bound n -> (n, n)
getBounds (Bound l u) = (getRecommend l, getRecommend u)

getBound :: HasBounds a => E (V a) -> a -> (N a, N a)
getBound e a = getBounds $ a ^. bounds . _Wrapped' . el e

-- boundsMin :: (V a ~ v, N a ~ n, Representable v, HasBounds a) => Lens' a (v (Recommend n))
-- boundsMin = bounds . _Wrapped' . column lowerBound

-- | Lens to the minimum point of a 'Bounds'.
boundsMin :: (InSpace v n a, Representable v, HasBounds a) => Lens' a (Point v n)
boundsMin = bounds . _Wrapped' . column (lowerBound . recommend) . iso P (\(P a) -> a)

-- | Lens to the maximum point of a 'Bounds'.
boundsMax :: (InSpace v n a, HasBasis v, HasBounds a) => Lens' a (Point v n)
boundsMax = bounds . _Wrapped' . column (upperBound . recommend) . iso P (\(P a) -> a)

-- Orientation

data Orientation = Horizontal | Verticle

-- Take two functions for each outcome for an orientation.
orient :: Orientation -> a -> a -> a
orient Horizontal h _ = h
orient Verticle   _ v = v

-- Legends

data LegendPic b n = DefaultLegendPic
                   | CustomLegendPic (ThemeEntry b n -> QDiagram b V2 n Any)

instance Default (LegendPic b n) where
  def = DefaultLegendPic

data LegendEntry b n = LegendEntry
  { _legendPic        :: LegendPic b n
  , _legendText       :: String
  , _legendPrecidence :: n
  } deriving Typeable

makeLenses ''LegendEntry

instance Num n => Default (LegendEntry b n) where
  def = LegendEntry
          { _legendPic        = def
          , _legendText       = ""
          , _legendPrecidence = 0
          }

-- Generic Plot info

-- | Data type for holding information all plots must contain.
data GenericPlot b v n = GenericPlot
  { _plotTransform   :: Transformation v n
  , _plotBounds      :: Bounds v n
  , _clipPlot        :: Bool
  , _plotThemeEntry  :: Recommend (ThemeEntry b n)
  , _legendEntries   :: [LegendEntry b n]
  , _plotName        :: Name
  , _plotBoundingBox :: BoundingBox v n
  } deriving Typeable

type instance B (GenericPlot b v n) = b
type instance V (GenericPlot b v n) = v
type instance N (GenericPlot b v n) = n

-- makeLensesWith (classyRules & generateSignatures .~ False) ''GenericPlot
-- makeClassy ''GenericPlot

-- I don't know how to give documenation for classes using TH, so I just wrote 
-- it by hand.

-- | Class that gives a lens onto a 'genericPlot'. All 'Plot's must impliment 
--   this class.
class HasGenericPlot t where

  {-# MINIMAL genericPlot #-}
  genericPlot :: Lens' t (GenericPlot (B t) (V t) (N t))

  -- | Clip anything outside the current axis bounds.
  clipPlot :: Lens' t Bool
  clipPlot = genericPlot . lens
    (\GenericPlot { _clipPlot = a } -> a)
    (\g a -> g { _clipPlot = a})
  {-# INLINE clipPlot #-}

  -- | The theme entry to be used for the current plot.
  plotThemeEntry :: Lens' t (Recommend (ThemeEntry (B t) (N t)))
  plotThemeEntry = genericPlot . lens
    (\GenericPlot { _plotThemeEntry = a } -> a)
    (\g a -> g { _plotThemeEntry = a})
  {-# INLINE plotThemeEntry #-}

  -- | The legend entries to be used for the current plot.
  legendEntries :: Lens' t [LegendEntry (B t) (N t)]
  legendEntries = genericPlot . lens
    (\GenericPlot { _legendEntries = a } -> a)
    (\g a -> g { _legendEntries = a})
  {-# INLINE legendEntries #-}

  -- | The bounds the current plot requests.
  plotBounds :: Lens' t (Bounds (V t) (N t))
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
  plotTransform :: Lens' t (Transformation (V t) (N t))
  plotTransform = genericPlot . lens
    (\GenericPlot { _plotTransform = a } -> a)
    (\g a -> g { _plotTransform = a})
  {-# INLINE plotTransform #-}

  -- | The transformation to be applied to the plot.
  plotBoundingBox :: Lens' t (BoundingBox (V t) (N t))
  plotBoundingBox = genericPlot . lens
    (\GenericPlot { _plotBoundingBox = a } -> a)
    (\g a -> g { _plotBoundingBox = a})
  {-# INLINE plotBoundingBox #-}

instance HasGenericPlot (GenericPlot b v n) where
  genericPlot = id
  {-# INLINE genericPlot #-}

-- Some orphan overlapping instances. Should be alright as long as these 
-- instances arn't defined for anything with HasGenericPlot elsewhere. The 
-- alternative is to have this all these instances defined for each plot or 
-- rewrite lenses specific to HasGenericPlot.

instance (HasGenericPlot a, N a ~ n, B a ~ b) => HasThemeEntry a b n where
  themeEntry = plotThemeEntry . recommend

-- | The style is applied to all theme styles. Only works for R2 due to 
--   HasStyle limitations.
instance (HasGenericPlot a, V a ~ V2, Typeable (N a)) => HasStyle a where
  applyStyle sty = over themeEntry
                 $ over themeLineStyle (applyStyle sty)
                 . over themeMarkerStyle (applyStyle sty)
                 . over themeFillStyle (applyStyle sty)

instance (Num (N a), HasGenericPlot a , HasLinearMap (V a)) => Transformable a where
  transform = over plotTransform . transform

instance HasGenericPlot a => HasBounds a where
  bounds = plotBounds

-- | Move origin by applying to @plotTransform@.
instance (Num (N a), HasGenericPlot a, HasLinearMap (V a)) => HasOrigin a where
  moveOriginTo = over plotTransform . moveOriginTo

instance HasGenericPlot a => Qualifiable a where
  n |> p = over plotName (n |>) p

-- | The @themeEntry@ lens goes though recommend, so @set themeEntry myTheme 
--   myPlot@ won't give a committed theme entry (so theme from axis will 
--   override). Use commitTheme to make sure theme is committed.
commitTheme :: HasGenericPlot a => ThemeEntry (B a) (N a) -> a -> a
commitTheme = set plotThemeEntry . Commit

-- | Make the current theme a committed theme. See @commitTheme@.
commitCurrentTheme :: HasGenericPlot a => a -> a
commitCurrentTheme = over plotThemeEntry makeCommitted
  where
    makeCommitted (Recommend a) = Commit a
    makeCommitted c             = c
    
zeroInt :: Additive v => v Int
zeroInt = zero

instance (TypeableFloat n, Renderable (Path V2 n) b, Additive v)
    => Default (GenericPlot b v n) where
  def = GenericPlot
          { _plotTransform   = mempty
          , _plotBounds      = Bounds $ def <$ zeroInt
          , _clipPlot        = True
          , _plotThemeEntry  = Recommend def
          , _legendEntries   = []
          , _plotName        = mempty
          , _plotBoundingBox = emptyBox
          }

instance (TypeableFloat n, Renderable (Path V2 n) b) => Default (GenericPlot b V3 n) where
  def = GenericPlot
          { _plotTransform   = mempty
          , _plotBounds      = Bounds $ pure def
          , _clipPlot        = True
          , _plotThemeEntry  = Recommend def
          , _legendEntries   = []
          , _plotName        = mempty
          , _plotBoundingBox = emptyBox
          }

-- Plot data type

-- | General class for something that can be wrapped in 'Plot'. The 'plot' 
--   function is rarely used by the end user.
class (HasGenericPlot a, Typeable a) => Plotable a where
  plot :: (V a ~ v, N a ~ n, B a ~ b)
       => v (n, n)
       -> Transformation v n
       -> (v n -> V2 n)
       -> T2 n
       -> a
       -> QDiagram b V2 n Any

  defLegendPic :: (N a ~ n, Ord n, Floating n) => a -> QDiagram (B a) V2 n Any
  defLegendPic = mempty

-- | Make a legend entry with the given string and default picture.
mkLegendEntry :: Num n => String -> LegendEntry b n
mkLegendEntry txt = def & legendText .~ txt

-- | Existential wrapper for something plotable.
data Plot b v n = forall a. (V a ~ v, N a ~ n, B a ~ b, Plotable a) => Plot a
  deriving Typeable

type instance B (Plot b v n) = b
type instance V (Plot b v n) = v
type instance N (Plot b v n) = n

instance (Typeable b, Typeable v, Typeable n) => Plotable (Plot b v n) where
  plot bs tv l t2 (Plot p) = plot bs tv l t2 p

  defLegendPic (Plot a) = defLegendPic a

instance HasGenericPlot (Plot b v n) where
  genericPlot = lens (\(Plot a)    -> view genericPlot a)
                     (\(Plot a) gp -> Plot (set genericPlot gp a))


-- | Prism onto the unwrapped plotable type. All standard plots export a 
--   specialised version of this which is normally more usefull (i.e. 
--   '_LinePlot').
_Plot :: Plotable a => Prism' (Plot (B a) (V a) (N a)) a
_Plot = prism' Plot (\(Plot a) -> cast a)

