{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverlappingInstances      #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE StandaloneDeriving        #-}

{-# LANGUAGE CPP                       #-}
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
  , boundsMin, boundsMax
  -- , zMin, zMax, zAxisBounds

  -- * Orientation
  , Orientation (..)
  , orient

  -- * Legend
  , mkLegendEntry
  , LegendEntry
  , LegendPic (..)
  , legendPic
  , legendText
  , legendPrecidence

  -- * Generic plot
  , PlotProperties
  , HasPlotProperties (..)

  -- * Plot / Plotable
  , Plotable (..)
  , Plot (..)
  , _Plot

  , B
  -- , plotLineStyle
  -- , plotMarkerStyle
  -- , plotFillStyle
  , recommend
  , _Recommend
  , _Commit
  ) where

import           Control.Lens          as L hiding (transform, ( # ), (|>))
import           Data.Default
import           Data.Functor.Rep
import           Data.Monoid.Recommend
import           Data.Typeable
import           Diagrams.BoundingBox
import           Diagrams.Prelude      as D hiding (view)

import           Linear
import           Plots.Themes
import           Plots.Utils

type family B a :: *

type instance B (QDiagram b v n m) = b

deriving instance (Show a) => Show (Recommend a)
deriving instance (Read a) => Read (Recommend a)
deriving instance (Eq a)   => Eq (Recommend a)
deriving instance (Ord a)  => Ord (Recommend a)

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

type instance V (LegendEntry b n) = V2
type instance N (LegendEntry b n) = n
type instance B (LegendEntry b n) = b

instance Num n => Default (LegendEntry b n) where
  def = LegendEntry
          { _legendPic        = def
          , _legendText       = ""
          , _legendPrecidence = 0
          }

mkLegendEntry :: Num n => String -> LegendEntry b n
mkLegendEntry x = LegendEntry DefaultLegendPic x 0

-- Generic Plot info

-- | Data type for holding information all plots must contain.
data PlotProperties b v n = PlotProperties
  { _plotTransform   :: Transformation v n
  , _plotBounds      :: Bounds v n
  , _clipPlot        :: Bool
  , _plotThemeEntry  :: Recommend (ThemeEntry b n)
  , _legendEntries   :: [LegendEntry b n]
  , _plotName        :: Name
  , _plotBoundingBox :: BoundingBox v n
  } deriving Typeable

type instance B (PlotProperties b v n) = b
type instance V (PlotProperties b v n) = v
type instance N (PlotProperties b v n) = n

-- makeLensesWith (classyRules & generateSignatures .~ False) ''PlotProperties
-- makeClassy ''PlotProperties

-- I don't know how to give documenation for classes using TH, so I just wrote
-- it by hand.

-- | Class that gives a lens onto a 'plotProperties'. All 'Plot's must impliment
--   this class.
class HasPlotProperties t where

  {-# MINIMAL plotProperties #-}
  plotProperties :: Lens' t (PlotProperties (B t) (V t) (N t))

  -- | Clip anything outside the current axis bounds.
  clipPlot :: Lens' t Bool
  clipPlot = plotProperties . lens
    (\PlotProperties { _clipPlot = a } -> a)
    (\g a -> g { _clipPlot = a})
  {-# INLINE clipPlot #-}

  -- | The theme entry to be used for the current plot.
  plotThemeEntry :: Lens' t (Recommend (ThemeEntry (B t) (N t)))
  plotThemeEntry = plotProperties . lens
    (\PlotProperties { _plotThemeEntry = a } -> a)
    (\g a -> g { _plotThemeEntry = a})
  {-# INLINE plotThemeEntry #-}

  -- | The legend entries to be used for the current plot.
  legendEntries :: Lens' t [LegendEntry (B t) (N t)]
  legendEntries = plotProperties . lens
    (\PlotProperties { _legendEntries = a } -> a)
    (\g a -> g { _legendEntries = a})
  {-# INLINE legendEntries #-}

  -- | The bounds the current plot requests.
  plotBounds :: Lens' t (Bounds (V t) (N t))
  plotBounds = plotProperties . lens
    (\PlotProperties { _plotBounds = a } -> a)
    (\g a -> g { _plotBounds = a})
  {-# INLINE plotBounds #-}

  -- | The name of the plot. This name is given to the rendered diagram.
  plotName :: Lens' t Name
  plotName = plotProperties . lens
    (\PlotProperties { _plotName = a } -> a)
    (\g a -> g { _plotName = a})
  {-# INLINE plotName #-}

  -- | The transformation to be applied to the plot.
  plotTransform :: Lens' t (Transformation (V t) (N t))
  plotTransform = plotProperties . lens
    (\PlotProperties { _plotTransform = a } -> a)
    (\g a -> g { _plotTransform = a})
  {-# INLINE plotTransform #-}

  -- -- | The transformation to be applied to the plot.
  -- plotBoundingBox :: Lens' t (BoundingBox (V t) (N t))
  -- plotBoundingBox = plotProperties . lens
  --   (\PlotProperties { _plotBoundingBox = a } -> a)
  --   (\g a -> g { _plotBoundingBox = a})
  -- {-# INLINE plotBoundingBox #-}

instance HasPlotProperties (PlotProperties b v n) where
  plotProperties = id
  {-# INLINE plotProperties #-}

-- Some orphan overlapping instances. Should be alright as long as these
-- instances arn't defined for anything with HasPlotProperties elsewhere. The
-- alternative is to have this all these instances defined for each plot or
-- rewrite lenses specific to HasPlotProperties.

instance (HasPlotProperties a, N a ~ n, B a ~ b) => HasThemeEntry a b n where
  themeEntry = plotThemeEntry . recommend

-- | The style is applied to all theme styles. Only works for R2 due to
--   HasStyle limitations.
instance (HasPlotProperties a, V a ~ V2, Typeable (N a)) => HasStyle a where
  applyStyle sty = over themeEntry
                 $ over themeLineStyle (applyStyle sty)
                 . over themeMarkerStyle (applyStyle sty)
                 . over themeFillStyle (applyStyle sty)

instance (Num (N a), HasPlotProperties a , HasLinearMap (V a)) => Transformable a where
  transform = over plotTransform . transform

instance HasPlotProperties a => HasBounds a where
  bounds = plotBounds

-- | Move origin by applying to @plotTransform@.
instance (Num (N a), HasPlotProperties a, HasLinearMap (V a)) => HasOrigin a where
  moveOriginTo = over plotTransform . moveOriginTo

instance HasPlotProperties a => Qualifiable a where
  n |> p = over plotName (n |>) p

zeroInt :: Additive v => v Int
zeroInt = zero

instance (TypeableFloat n, Renderable (Path V2 n) b, Additive v)
    => Default (PlotProperties b v n) where
  def = PlotProperties
          { _plotTransform   = mempty
          , _plotBounds      = Bounds $ def <$ zeroInt
          , _clipPlot        = True
          , _plotThemeEntry  = Recommend def
          , _legendEntries   = []
          , _plotName        = mempty
          , _plotBoundingBox = emptyBox
          }

------------------------------------------------------------------------
-- Plotable
------------------------------------------------------------------------

-- | General class for something that can be wrapped in 'Plot'. The 'plot'
--   function is rarely used by the end user.
class (Typeable a, Enveloped a) => Plotable a b where
  renderPlotable :: InSpace v n a
    => v (n, n)
    -> Transformation v n
    -> a
    -> PlotProperties b v n
    -> QDiagram b v n Any

  defLegendPic :: (InSpace v n a, OrderedField n)
    => a -> PlotProperties b v n -> QDiagram b V2 n Any
  defLegendPic = mempty

deriving instance Typeable Any

instance (Typeable b, Typeable v, Metric v, Typeable n, OrderedField n)
  => Plotable (QDiagram b v n Any) b where
  renderPlotable _ t dia _ = dia # transform t

------------------------------------------------------------------------
-- Plot wrapper
------------------------------------------------------------------------

-- | Existential wrapper for something plotable.
-- data Plot b v n = forall a. (V a ~ v, N a ~ n, Plotable a b) => Plot a (PlotProperties b v n)
--   deriving Typeable

data Plot b v n where
  Plot :: (V a ~ v, N a ~ n, Plotable a b)
       => a -> Plot b v n
  deriving (Typeable)

type instance B (Plot b v n) = b
type instance V (Plot b v n) = v
type instance N (Plot b v n) = n

instance (Metric v, OrderedField n) => Enveloped (Plot b v n) where
  getEnvelope (Plot a) = getEnvelope a

instance (Metric v, OrderedField n, Typeable (Plot b v n)) => Plotable (Plot b v n) b where
  renderPlotable bs t (Plot a) pp = renderPlotable bs t a pp
  defLegendPic (Plot a) pp        = defLegendPic a pp

-- instance HasPlotProperties (Plot b v n) where
--   plotProperties = lens (\(Plot _ pp)   -> pp)
--                         (\(Plot a _) pp -> Plot a pp)

-- | Prism onto the unwrapped plotable type. All standard plots export a
--   specialised version of this which is normally more usefull (i.e.
--   '_LinePlot').
-- _Plot :: (Typeable (N a), Typeable (V a), Typeable b, Plotable a b)
--       => Prism' (Plot b (V a) (N a)) (a, PlotProperties b (V a) (N a))
-- _Plot = prism' (uncurry Plot) (\(Plot a b) -> cast (a,b))

_Plot :: Plotable a b => Prism' (Plot b (V a) (N a)) a
_Plot = prism' Plot (\(Plot a) -> cast a)

-- plotT :: (Typeable (N a), Typeable (V a), Typeable b, Plotable a b)
--       => Traversal' (Plot b (V a) (N a)) a
-- plotT = _Plot . _1

