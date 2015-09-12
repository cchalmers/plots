{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Plots.Types
  (
  -- * Base space
    BaseSpace

  -- * Bounds
  , Bound (..)
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
  , HasOrientation (..)
  , orient
  , horizontal
  , vertical

  -- * Legend
  , mkLegendEntry
  , LegendEntry
  , LegendPic (..)
  , legendPic
  , legendText
  , legendPrecedence

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
  -- , _Plot

  , _recommend
  , _Recommend
  , _Commit

  , PropertiedPlot (..)
  , _pp

  , ModifiedPlot (..)
  , _Plot
  , modifyPlot
  , properties
  -- , appPlot'
  ) where

import           Data.Bool
import           Data.Functor.Rep
import           Data.Monoid.Recommend
import           Data.Orphans               ()
import           Data.Typeable
import           Diagrams.BoundingBox
import           Diagrams.Prelude           as D

#if __GLASGOW_HASKELL__ < 710
import           Data.Foldable           (Foldable)
#endif

import           Diagrams.Coordinates.Polar
import           Linear
import           Plots.Themes
import           Plots.Utils

-- | This family is used so that we can say (Axis Polar) but use V2 for the
--   underlying diagram.
type family BaseSpace (c :: * -> *) :: * -> *

type instance BaseSpace V2    = V2
type instance BaseSpace Polar = V2
type instance BaseSpace V3    = V3

-- Bounds

-- | A 'Bound' allows you to 'Commit' an upper or lower bound while having a
--   fallback 'Recommend' value.
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

class HasBounds a c | a -> c where
  bounds :: Lens' a (Bounds c (N a))

instance HasBounds (Bounds V2 n) V2 where
  bounds = id
  {-# INLINE bounds #-}

instance HasBounds (Bounds V3 n) V3 where
  bounds = id
  {-# INLINE bounds #-}

getBounds :: Bound n -> (n, n)
getBounds (Bound l u) = (getRecommend l, getRecommend u)

getBound :: HasBounds a c => E c -> a -> (N a, N a)
getBound e a = getBounds $ a ^. bounds . _Wrapped' . el e

-- boundsMin :: (V a ~ v, N a ~ n, Representable v, HasBounds a) => Lens' a (v (Recommend n))
-- boundsMin = bounds . _Wrapped' . column lowerBound

-- | Lens to the minimum point of a 'Bounds'.
boundsMin :: (InSpace v n a, Representable v, HasBounds a v) => Lens' a (Point v n)
boundsMin = bounds . _Wrapped' . column (lowerBound . _recommend) . iso P (\(P a) -> a)

-- | Lens to the maximum point of a 'Bounds'.
boundsMax :: (InSpace v n a, HasBasis v, HasBounds a v) => Lens' a (Point v n)
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

data Orientation = Horizontal | Vertical
  deriving (Show, Eq, Ord, Typeable)

-- Take two functions for each outcome for an orientation.
orient :: HasOrientation o => o -> a -> a -> a
orient o h v =
  case view orientation o of
    Horizontal -> h
    Vertical   -> v

-- | Class of things that have an orientation.
class HasOrientation a where
  -- | Lens onto the orientation of an object.
  orientation :: Lens' a Orientation

instance HasOrientation Orientation where
  orientation = id

-- | Lens onto whether an object's orientation is horizontal.
horizontal :: HasOrientation a => Lens' a Bool
horizontal = orientation . iso (==Horizontal) (bool Vertical Horizontal)

-- | Lens onto whether an object's orientation is vertical.
vertical :: HasOrientation a => Lens' a Bool
vertical = horizontal . involuted not

-- Legends

-- | Type allowing use of the default legend picture (depending on the
--   plot) or a custom legend picture with access to the 'PlotStyle'.
data LegendPic b v n
  = DefaultLegendPic
  | CustomLegendPic (PlotStyle b v n -> QDiagram b v n Any)

instance Default (LegendPic b v n) where
  def = DefaultLegendPic

-- | Data type for holding a legend entry.
data LegendEntry b v n = LegendEntry
  { _legendPic        :: LegendPic b v n
  , _legendText       :: String
  , _legendPrecedence :: n
  } deriving Typeable

makeLenses ''LegendEntry

type instance V (LegendEntry b v n) = v
type instance N (LegendEntry b v n) = n

-- | Make a legend entry with a default 'legendPic' and
--   'legendPrecedence' 0 using the string as the 'legendText'.
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
  , _plotColourMap   :: ColourMap
  , _plotBoundingBox :: BoundingBox v n
  } deriving Typeable

type instance V (PlotProperties b v n) = v
type instance N (PlotProperties b v n) = n

-- makeLensesWith (classyRules & generateSignatures .~ False) ''PlotProperties
-- makeClassy ''PlotProperties

-- I don't know how to give documentation for classes using TH, so I just wrote
-- it by hand.

type BaseV t = BaseSpace (V t)

-- | Class that gives a lens onto a 'plotProperties'. All 'Plot's must implement
--   this class.
class HasPlotProperties t b | t -> b where

  {-# MINIMAL plotProperties #-}
  plotProperties :: Lens' t (PlotProperties b (V t) (N t))

  -- | Clip anything outside the current axis bounds.
  clipPlot :: Lens' t Bool
  clipPlot = plotProperties . lens _clipPlot (\g a -> g { _clipPlot = a})
  {-# INLINE clipPlot #-}

  -- | The theme entry to be used for the current plot.
  plotPropertiesStyle :: Lens' t (PlotStyle b (V t) (N t))
  plotPropertiesStyle = plotProperties . lens _plotStyle (\g a -> g { _plotStyle = a})
  {-# INLINE plotPropertiesStyle #-}

  -- | The legend entries to be used for the current plot.
  legendEntries :: Lens' t [LegendEntry b (V t) (N t)]
  legendEntries = plotProperties . lens _legendEntries (\g a -> g { _legendEntries = a})
  {-# INLINE legendEntries #-}

  -- | The colour map to use to render this plot (for
  --   'Plots.Axis.Heatmap.HeatMap' like plots)
  plotColourMap :: Lens' t ColourMap
  plotColourMap = plotProperties . lens _plotColourMap (\g a -> g { _plotColourMap = a})
  {-# INLINE plotColourMap #-}

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

instance HasPlotProperties (PlotProperties b v n) b where
  plotProperties = id
  {-# INLINE plotProperties #-}

-- Some orphan overlapping instances. Should be alright as long as these
-- instances aren't defined for anything with HasPlotProperties elsewhere. The
-- alternative is to have all these instances defined for each plot or
-- rewrite lenses specific to HasPlotProperties.

instance (HasLinearMap v, Num n) => Transformable (PlotProperties b v n) where
  transform = over plotTransform . transform

instance HasBounds (PlotProperties b v n) v where
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
    , _plotColourMap   = hot
    , _plotStyle       = mempty
    , _plotBoundingBox = emptyBox
    }

------------------------------------------------------------------------
-- Plotable
------------------------------------------------------------------------

-- | Size information give to a 'Plotable' before rendering.
data AxisSpec v n = AxisSpec
  { _specBounds :: v (n, n)
  , _specTrans  :: Transformation v n
  , _specScale  :: v AxisScale
  }

makeLenses ''AxisSpec

type instance V (AxisSpec v n) = v
type instance N (AxisSpec v n) = n

-- | Scale a number by log10-ing it and linearly scaling it so it's
--   within the same range.
scaleNum :: Floating n => (n, n) -> AxisScale -> n -> n
scaleNum (a,b) s x = case s of
  LinearAxis -> x
  LogAxis    -> subtract a $ (b / logBase 10 d) * (logBase 10 x)
    where d = b - a

-- | Apply log scaling and the transform to a point.
specPoint :: (Applicative v, Additive v, Floating n) => AxisSpec v n -> Point v n -> Point v n
specPoint (AxisSpec bs tr ss) p =
  papply tr $ over _Point (scaleNum <$> bs <*> ss <*>) p

-- | General class for something that can be wrapped in 'Plot'. The 'plot'
--   function is rarely used by the end user.
class (Typeable a, Enveloped a) => Plotable a b where
  -- | Render a plot using the 'AxisSpec' to properly position and scale
  --   the plot and 'PlotProperties' for style aspects.
  renderPlotable
    :: InSpace v n a
    => AxisSpec v n
    -> a
    -> PlotProperties b v n
    -> QDiagram b v n Any

  -- | The default legened picture when the 'LegendPic' is
  --   'DefaultLegendPic'.
  defLegendPic :: (InSpace v n a, OrderedField n)
    => a -> PlotProperties b v n -> QDiagram b V2 n Any
  defLegendPic = mempty

instance (Typeable b, Typeable v, Metric v, Typeable n, OrderedField n)
  => Plotable (QDiagram b v n Any) b where
  renderPlotable s dia _ = dia # transform (s^.specTrans)

------------------------------------------------------------------------
-- Plot wrapper
------------------------------------------------------------------------

-- | Existential wrapper for something plotable. This only contains the
--   raw infomation for the plot, not the 'PlotProperties'.
data Plot b v n where
  Plot :: (V a ~ v, N a ~ n, Plotable a b) => a -> Plot b v n
  deriving Typeable

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

-- | Lens onto the plot inside a 'PropertiedPlot'.
_pp :: (V p ~ V p', N p ~ N p') => Lens (PropertiedPlot p b) (PropertiedPlot p' b) p p'
_pp = lens (\(PP a _) -> a) (\(PP _ p) a -> PP a p)

type instance V (PropertiedPlot p b) = V p
type instance N (PropertiedPlot p b) = N p

instance HasPlotProperties (PropertiedPlot p b) b where
  plotProperties = lens (\(PP _ pp) -> pp) (\(PP a _) pp -> PP a pp)

-- | Internal type for storing plots in an axis. Using an 'Endo' of the
--   'PropertiedPlot' allows more flexibility.
data ModifiedPlot b v n where
  ModifiedPlot :: (V a ~ v, N a ~ n, Plotable a b)
       => a -> Endo (PropertiedPlot a b) -> ModifiedPlot b v n
  deriving Typeable

type instance V (ModifiedPlot b v n) = v
type instance N (ModifiedPlot b v n) = n

-- | Traversal over the plot of a modified plot.
_ModifiedPlot :: forall a b v n. Plotable a b => Traversal' (ModifiedPlot b v n) a
_ModifiedPlot f p@(ModifiedPlot a e) =
  case eq a of
    Just Refl -> f a <&> \b' -> ModifiedPlot b' e
    Nothing   -> pure p
  where
  eq :: Typeable a' => a' -> Maybe (a :~: a')
  eq _ = eqT

modifyPlot :: ModifiedPlot b v n -> PlotProperties b v n -> (Plot b v n, PlotProperties b v n)
modifyPlot (ModifiedPlot a (Endo pf)) pp = (Plot a', pp')
  where
    PP a' pp' = pf $ PP a pp

-- | Setter over the plot properties.
properties :: Setter' (ModifiedPlot b v n) (PlotProperties b v n)
properties = sets $ \f -> appPlot' $ \(PP a pp) -> PP a (f pp) -- g

appPlot' :: (forall a. (V a ~ v, N a ~ n, Plotable a b) =>
             PropertiedPlot a b -> PropertiedPlot a b)
         -> ModifiedPlot b v n -> ModifiedPlot b v n
appPlot' f (ModifiedPlot a pf) = ModifiedPlot a (pf <> Endo f)

instance (HasLinearMap (V p), Num (N p)) => Transformable (PropertiedPlot p b) where
  transform = over plotTransform . transform

instance v ~ V p => HasBounds (PropertiedPlot p b) v where
  bounds = plotBounds

-- | Move origin by applying to @plotTransform@.
instance (BaseV p ~ V p, Additive (V p), Num (N p)) => HasOrigin (PropertiedPlot p b) where
  moveOriginTo = over plotTransform . moveOriginTo

instance BaseV p ~ V p => HasPlotStyle (PropertiedPlot p b) b where
  plotStyle = plotPropertiesStyle

instance Qualifiable (PropertiedPlot p b) where
  n .>> p = over plotName (n .>>) p

instance (Metric v, OrderedField n) => Enveloped (ModifiedPlot b v n) where
  getEnvelope (ModifiedPlot p _) = getEnvelope p

