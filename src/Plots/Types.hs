{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables #-}


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

  -- * Logarithmic scaling
  , AxisScale (..)
  , logNumber
  , logPoint
  , logDeform

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

  -- * Plot spec
  , AxisSpec (..)
  , specTrans
  , specBounds
  , specScale
  , scaleNum
  , specPoint

  -- * Plot / Plotable
  , Plotable (..)
  , Plot (..)
  , _Plot

  , B
  -- , plotLineStyle
  -- , plotMarkerStyle
  -- , plotFillStyle
  , _recommend
  , _Recommend
  , _Commit

  , PropertiedPlot (..)
  , _pp

  , Plot' (..)
  , _Plot'
  , unPlot'
  , appPlot'
  ) where

-- import           Control.Lens          as L hiding (transform, ( # ), (|>))
-- import           Data.Default
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
boundsMin = bounds . _Wrapped' . column (lowerBound . _recommend) . iso P (\(P a) -> a)

-- | Lens to the maximum point of a 'Bounds'.
boundsMax :: (InSpace v n a, HasBasis v, HasBounds a) => Lens' a (Point v n)
boundsMax = bounds . _Wrapped' . column (upperBound . _recommend) . iso P (\(P a) -> a)

-- Logarithmic scaling -------------------------------------------------

-- | Should the axis be on a logarithmic scale.
data AxisScale = LinearAxis | LogAxis
  deriving (Show, Eq)

-- | Log the number for 'LogAxis', do nothing for 'LinearAxis'.
logNumber :: Floating a => AxisScale -> a -> a
logNumber LinearAxis = id
logNumber LogAxis    = log
{-# INLINE logNumber #-}

-- | Transform a point according to the axis scale. Does nothing for
--   linear scales.
logPoint :: (Additive v, Floating n) => v AxisScale -> Point v n -> Point v n
logPoint v = _Point %~ liftI2 logNumber v
{-# INLINE logPoint #-}

-- | Deform an object according to the axis scale. Does nothing for
--   linear scales.
logDeform :: (InSpace v n a, Additive v, Foldable v, Floating n, Deformable a a)
          => v AxisScale -> a -> a
logDeform v
  | allOf folded (== LinearAxis) v = id
  | otherwise                      = deform (Deformation $ logPoint v)

instance Default AxisScale where
  def = LinearAxis

-- Orientation ---------------------------------------------------------

data Orientation = Horizontal | Verticle

-- Take two functions for each outcome for an orientation.
orient :: Orientation -> a -> a -> a
orient Horizontal h _ = h
orient Verticle   _ v = v

-- Legends

data LegendPic b v n = DefaultLegendPic
                     | CustomLegendPic (PlotStyle b v n -> QDiagram b v n Any)

instance Default (LegendPic b v n) where
  def = DefaultLegendPic

data LegendEntry b v n = LegendEntry
  { _legendPic        :: LegendPic b v n
  , _legendText       :: String
  , _legendPrecidence :: n
  } deriving Typeable

makeLenses ''LegendEntry

type instance V (LegendEntry b v n) = v
type instance N (LegendEntry b v n) = n
type instance B (LegendEntry b v n) = b

mkLegendEntry :: Num n => String -> LegendEntry b v n
mkLegendEntry x = LegendEntry DefaultLegendPic x 0

-- Generic Plot info

-- | Data type for holding information all plots must contain.
data PlotProperties b v n = PlotProperties
  { _plotTransform   :: Transformation v n
  , _plotBounds      :: Bounds v n
  , _clipPlot        :: Bool
  , _legendEntries   :: [LegendEntry b v n]
  , _plotName        :: Name
  , _plotStyle       :: PlotStyle b v n
  , _plotBoundingBox :: BoundingBox v n
  } deriving Typeable

type instance B (PlotProperties b v n) = b
type instance V (PlotProperties b v n) = v
type instance N (PlotProperties b v n) = n

-- makeLensesWith (classyRules & generateSignatures .~ False) ''PlotProperties
-- makeClassy ''PlotProperties

-- I don't know how to give documentation for classes using TH, so I just wrote
-- it by hand.

-- | Class that gives a lens onto a 'plotProperties'. All 'Plot's must impliment
--   this class.
class HasPlotProperties t where

  {-# MINIMAL plotProperties #-}
  plotProperties :: Lens' t (PlotProperties (B t) (V t) (N t))

  -- | Clip anything outside the current axis bounds.
  clipPlot :: Lens' t Bool
  clipPlot = plotProperties . lens _clipPlot (\g a -> g { _clipPlot = a})
  {-# INLINE clipPlot #-}

  -- | The theme entry to be used for the current plot.
  plotPropertiesStyle :: Lens' t (PlotStyle (B t) (V t) (N t))
  plotPropertiesStyle = plotProperties . lens _plotStyle (\g a -> g { _plotStyle = a})
  {-# INLINE plotPropertiesStyle #-}

  -- | The legend entries to be used for the current plot.
  legendEntries :: Lens' t [LegendEntry (B t) (V t) (N t)]
  legendEntries = plotProperties . lens _legendEntries (\g a -> g { _legendEntries = a})
  {-# INLINE legendEntries #-}

  -- | The bounds the current plot requests.
  plotBounds :: Lens' t (Bounds (V t) (N t))
  plotBounds = plotProperties . lens _plotBounds (\g a -> g { _plotBounds = a})
  {-# INLINE plotBounds #-}

  -- | The name of the plot. This name is given to the rendered diagram.
  plotName :: Lens' t Name
  plotName = plotProperties . lens _plotName (\g a -> g { _plotName = a})
  {-# INLINE plotName #-}

  -- | The transformation to be applied to the plot.
  plotTransform :: Lens' t (Transformation (V t) (N t))
  plotTransform = plotProperties . lens _plotTransform (\g a -> g { _plotTransform = a})
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

instance (HasLinearMap v, Num n) => Transformable (PlotProperties b v n) where
  transform = over plotTransform . transform

instance HasBounds (PlotProperties b v n) where
  bounds = plotBounds

-- | Move origin by applying to @plotTransform@.
instance (Additive v, Num n) => HasOrigin (PlotProperties b v n) where
  moveOriginTo = over plotTransform . moveOriginTo

instance HasPlotStyle (PlotProperties b v n) b where
  plotStyle = plotPropertiesStyle

instance Qualifiable (PlotProperties b v n) where
  n .>> p = over plotName (n .>>) p

zeroInt :: Additive v => v Int
zeroInt = zero

instance (TypeableFloat n, Renderable (Path V2 n) b, Metric v)
    => Default (PlotProperties b v n) where
  def = PlotProperties
          { _plotTransform   = mempty
          , _plotBounds      = Bounds $ def <$ zeroInt
          , _clipPlot        = True
          , _legendEntries   = []
          , _plotName        = mempty
          , _plotStyle       = mempty
          , _plotBoundingBox = emptyBox
          }

------------------------------------------------------------------------
-- Plotable
------------------------------------------------------------------------

-- | Data given to a 'Plotable' before rendering.
data AxisSpec v n = AxisSpec
  -- I need to get better an naming things.
  { _specBounds :: v (n, n)
  , _specTrans  :: Transformation v n
  , _specScale  :: v AxisScale
  }

makeLenses ''AxisSpec

-- | Scale a number by log10-ing it and linearly scaleing it so it's
--    within the same range.
scaleNum :: Floating n => (n, n) -> AxisScale -> n -> n
scaleNum (a,b) s x = case s of
  LinearAxis -> x
  LogAxis    -> subtract a $ (b / logBase 10 d) * (logBase 10 x)
    where d = b - a

-- | Apply log scalling and the transform to a point.
specPoint :: (Applicative v, Additive v, Floating n) => AxisSpec v n -> Point v n -> Point v n
specPoint (AxisSpec bs tr ss) p =
  papply tr $ over _Point (scaleNum <$> bs <*> ss <*>) p

-- | General class for something that can be wrapped in 'Plot'. The 'plot'
--   function is rarely used by the end user.
class (Typeable a, Enveloped a) => Plotable a b where
  renderPlotable
    :: InSpace v n a
    => AxisSpec v n
    -> a
    -> PlotProperties b v n
    -> QDiagram b v n Any

  defLegendPic :: (InSpace v n a, OrderedField n)
    => a -> PlotProperties b v n -> QDiagram b V2 n Any
  defLegendPic = mempty

deriving instance Typeable Any

instance (Typeable b, Typeable v, Metric v, Typeable n, OrderedField n)
  => Plotable (QDiagram b v n Any) b where
  renderPlotable s dia _ = dia # transform (s^.specTrans)

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
  renderPlotable s (Plot a) pp = renderPlotable s a pp
  defLegendPic (Plot a) pp     = defLegendPic a pp

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

-- The main perpose of a 'PropertiedPlot' is so we can use lenses from
-- both the plot its self and the plots properites.
data PropertiedPlot p b = PP p (PlotProperties b (V p) (N p))

_pp :: (V p ~ V p', N p ~ N p') => Lens (PropertiedPlot p b) (PropertiedPlot p' b) p p'
_pp = lens (\(PP a _) -> a) (\(PP _ p) a -> PP a p)

type instance B (PropertiedPlot p b) = b
type instance V (PropertiedPlot p b) = V p
type instance N (PropertiedPlot p b) = N p

instance HasPlotProperties (PropertiedPlot p b) where
  plotProperties = lens (\(PP _ pp) -> pp) (\(PP a _) pp -> PP a pp)

-- | Internal type for storing plots in an axis.
data Plot' b v n where
  Plot' :: (V a ~ v, N a ~ n, Plotable a b)
       => a -> Endo (PropertiedPlot a b) -> Plot' b v n
  deriving Typeable


_Plot' :: forall a b v n. Plotable a b => Traversal' (Plot' b v n) a
_Plot' f p@(Plot' a e) =
  case eq a of
    Just Refl -> f a <&> \b' -> Plot' b' e
    Nothing   -> pure p
  where
  eq :: Typeable a' => a' -> Maybe (a :~: a')
  eq _ = eqT

unPlot' :: Plot' b v n -> PlotProperties b v n -> (Plot b v n, PlotProperties b v n)
unPlot' (Plot' a (Endo pf)) pp = (Plot a', pp')
  where
    PP a' pp' = pf $ PP a pp

appPlot' :: (forall a. (V a ~ v, N a ~ n, Plotable a b) =>
             PropertiedPlot a b -> PropertiedPlot a b)
         -> Plot' b v n -> Plot' b v n
appPlot' f (Plot' a pf) = Plot' a (pf <> Endo f)

instance (HasLinearMap (V p), Num (N p)) => Transformable (PropertiedPlot p b) where
  transform = over plotTransform . transform

instance HasBounds (PropertiedPlot p b) where
  bounds = plotBounds

-- | Move origin by applying to @plotTransform@.
instance (Additive (V p), Num (N p)) => HasOrigin (PropertiedPlot p b) where
  moveOriginTo = over plotTransform . moveOriginTo

instance HasPlotStyle (PropertiedPlot p b) b where
  plotStyle = plotPropertiesStyle

instance Qualifiable (PropertiedPlot p b) where
  n .>> p = over plotName (n .>>) p
