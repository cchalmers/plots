{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Plots.Types
-- Copyright   :  (C) 2015 Christopher Chalmers
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Christopher Chalmers
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Primitive types for building plots.
--
----------------------------------------------------------------------------
module Plots.Types
  (
    -- * Plot type
    Plot
  , mkPlot
  , rawPlot

    -- ** Plot modifications
  , PlotMods
  , plotMods

    -- * Plot options
  , PlotOptions
  , HasPlotOptions (..)

    -- * Plotable class
  , Plotable (..)

  -- * Miscellaneous

  -- * Bounds
  , Bound (..)
  , Bounds (..)
  , HasBounds (..)
  , getBounds
  , getBound
  , upperBound
  , lowerBound
  , boundsMin, boundsMax

  -- * Logarithmic scaling
  , AxisScale (..)
  , logNumber
  , logPoint
  , logDeform

  -- * Visibility
  , HasVisibility (..)

  -- * Orientation
  , Orientation (..)
  , HasOrientation (..)
  , orient
  , horizontal
  , vertical

  -- * Legend
  , LegendEntry
  , LegendPic (..)
  , mkLegendEntry
  , legendPicture
  , legendText
  , legendPrecedence

    -- * Internal Plot types

    -- ** Axis spec
  , AxisSpec (..)
  , specTrans
  , specBounds
  , specScale
  , scaleNum
  , specPoint
  , specColourMap

    -- ** Dynamic plot
  , DynamicPlot (..)
  , _DynamicPlot
  , dynamicPlotMods

    -- ** Styled plot
  , StyledPlot
  , styleDynamic
  , renderStyledPlot
  , singleStyledPlotLegend
  , styledPlotLegends

  ) where

import           Data.Bool
import           Data.Functor.Rep
import           Data.Monoid.Recommend
import           Data.Orphans               ()
import           Data.Typeable
-- import           Diagrams.BoundingBox
import           Diagrams.Prelude           as D
import Data.List (sortOn)

#if __GLASGOW_HASKELL__ < 710
import           Data.Foldable           (Foldable)
#endif

import           Linear
import           Plots.Style
import           Plots.Utils

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

------------------------------------------------------------------------
-- Legend entries
------------------------------------------------------------------------

-- | Type allowing use of the default legend picture (depending on the
--   plot) or a custom legend picture with access to the 'PlotStyle'.
data LegendPic b v n
  = DefaultLegendPic
  | CustomLegendPic (PlotStyle b v n -> QDiagram b v n Any)

instance Default (LegendPic b v n) where
  def = DefaultLegendPic

-- | Data type for holding a legend entry.
data LegendEntry b v n = LegendEntry
  { lPic        :: LegendPic b v n
  , lText       :: String
  , lPrecedence :: n
  } deriving Typeable

-- | The picture used in the legend entry.
legendPicture :: Lens' (LegendEntry b v n) (LegendPic b v n)
legendPicture = lens lPic (\l pic -> l {lPic = pic})

-- | The text used in the legend entry.
legendText :: Lens' (LegendEntry b v n) String
legendText = lens lText (\l txt -> l {lText = txt})

-- | The order in which the legend entries are rendered. If precedence
--   are equal, they entries are put in the order they are added to the
--   axis.
--
--   Default is @0@.
legendPrecedence :: Lens' (LegendEntry b v n) n
legendPrecedence = lens lPrecedence (\l n -> l {lPrecedence = n})

type instance V (LegendEntry b v n) = v
type instance N (LegendEntry b v n) = n

-- | Make a legend entry with a default 'legendPicture' and
--   'legendPrecedence' 0 using the string as the 'legendText'.
mkLegendEntry :: Num n => String -> LegendEntry b v n
mkLegendEntry x = LegendEntry DefaultLegendPic x 0

------------------------------------------------------------------------
-- Plot attributes
------------------------------------------------------------------------

-- Generic Plot info

-- | Data type for holding information all plots must contain.
data PlotOptions b v n = PlotOptions
  { poName                :: Name
  , poClipPlot            :: Bool
  , poLegend              :: [LegendEntry b v n]
  , poVisible             :: Bool
  , poTransform           :: Transformation v n
  -- , poPostPlotBoundingBox :: BoundingBox v n -> BoundingBox v n
  -- , poPlotPostProduction  :: QDiagram b v n Any -> QDiagram b v n Any
  } deriving Typeable

type instance V (PlotOptions b v n) = v
type instance N (PlotOptions b v n) = n

-- | Class of things that have 'PlotOptions'.
class HasPlotOptions f a b | a -> b where
  {-# MINIMAL plotOptions #-}
  plotOptions :: LensLike' f a (PlotOptions b (V a) (N a))

  -- | The 'Name' applied to the plot. This gives a way to reference a
  --   specific plot in a rendered axis.
  --
  --   'Default' is 'mempty'.
  plotName :: Functor f => LensLike' f a Name
  plotName = plotOptions . lens poName (\g a -> g { poName = a})
  {-# INLINE plotName #-}

  -- | Whether the plot should be clipped to the bounds of the axes.
  --
  --   'Default' is 'True'.
  clipPlot :: Functor f => LensLike' f a Bool
  clipPlot = plotOptions . lens poClipPlot (\g a -> g {poClipPlot = a})
  {-# INLINE clipPlot #-}

  -- -- | The plot style to be used for the current plot.
  -- plotStyle :: Functor f => LensLike' f a (PlotStyle b (V t) (N t))
  -- plotStyle = plotOptions . lens poStyle (\g a -> g {paStyle = a})
  -- {-# INLINE plotOptionsStyle #-}

  -- | The legend entries to be used for the current plot.
  --
  --   'Default' is 'mempty'.
  legendEntries :: Functor f => LensLike' f a [LegendEntry b (V a) (N a)]
  legendEntries = plotOptions . lens poLegend (\g a -> g {poLegend = a})
  {-# INLINE legendEntries #-}

  -- | The transform applied to the plot once it's in the axis
  --   coordinates.
  --
  --   'Default' is 'mempty'.
  plotTransform :: Functor f
                     => LensLike' f a (Transformation (V a) (N a))
  plotTransform = plotOptions . lens poTransform (\g a -> g {poTransform = a})
  {-# INLINE plotTransform #-}

  -- | Whether or not the plot should be shown. The 'BoundingBox' of the
  --   plot will still affect the inferred axis bounds.
  --
  --   'Default' is 'True'.
  plotVisible :: Functor f => LensLike' f a Bool
  plotVisible = plotOptions . lens poVisible (\po b -> po { poVisible = b})
  {-# INLINE plotVisible #-}

instance (Additive v, Num n) => Default (PlotOptions b v n) where
  def = PlotOptions
    { poName                = mempty
    , poClipPlot            = True
    , poLegend              = []
    , poVisible             = True
    , poTransform           = mempty
    -- , poPostPlotBoundingBox = id
    -- , poPlotPostProduction  = id
    }

instance HasPlotOptions f (PlotOptions b v n) b where
  plotOptions = id
  {-# INLINE plotOptions #-}

instance (HasLinearMap v, Num n) => Transformable (PlotOptions b v n) where
  transform = over plotTransform . transform

-- instance HasBounds (PlotOptions b v n) v where
--   bounds = plotBounds

-- | Move origin by applying to @plotTransform@.
instance (Additive v, Num n) => HasOrigin (PlotOptions b v n) where
  moveOriginTo = over plotTransform . moveOriginTo

-- instance HasPlotStyle (PlotOptions b v n) b where
--   plotStyle = plotPropertiesStyle

instance Qualifiable (PlotOptions b v n) where
  n .>> p = over plotName (n .>>) p

-- instance HasVisibility (PlotOptions b v n) where
--   visible = plotVisible

-- zeroInt :: Additive v => v Int
-- zeroInt = zero

------------------------------------------------------------------------
-- AxisSpec
------------------------------------------------------------------------

-- | Information from the 'Plots.Axis.Axis' necessary to render a 'Plotable'.
data AxisSpec v n = AxisSpec
  { _specBounds :: v (n, n)
  , _specTrans  :: Transformation v n
  , _specScale  :: v AxisScale
  , _specColourMap :: ColourMap
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
specPoint (AxisSpec bs tr ss _) p =
  papply tr $ over _Point (scaleNum <$> bs <*> ss <*>) p

------------------------------------------------------------------------
-- Plotable class
------------------------------------------------------------------------

-- | General class for something that can be wrapped in 'Plot'. The 'plot'
--   function is rarely used by the end user.
class (Typeable p, Enveloped p) => Plotable p b where
  -- | Render a plot using the 'AxisSpec' to properly position and scale
  --   the plot and 'PlotOptions' for style aspects.
  renderPlotable
    :: InSpace v n p
    => AxisSpec v n
    -> PlotOptions b v n
    -> PlotStyle b v n
    -> p
    -> QDiagram b v n Any

  -- | The default legend picture when the 'LegendPic' is
  --   'DefaultLegendPic'.
  defLegendPic :: (InSpace v n p, OrderedField n)
    => p
    -> PlotStyle b v n
    -> QDiagram b v n Any
  defLegendPic = mempty

instance (Typeable b, Typeable v, Metric v, Typeable n, OrderedField n)
  => Plotable (QDiagram b v n Any) b where
  renderPlotable s _ _ dia = dia # transform (s^.specTrans)

------------------------------------------------------------------------
-- Visibility
------------------------------------------------------------------------

-- | Class of objects that can be hidden.
class HasVisibility a where
  -- | Lens onto whether an object should be visible when rendered.
  visible :: Lens' a Bool

  -- | The opposite of 'visible'.
  hidden :: Lens' a Bool
  hidden = visible . involuted not
  {-# INLINE hidden #-}

------------------------------------------------------------------------
-- Plot wrapper
------------------------------------------------------------------------

-- XXX Currently unused
-- Could be used in future to group plotables together to use the same
-- plot style and options.

-- -- | Existential wrapper for something plotable. This only contains the
-- --   raw infomation for the plot, not the 'PlotOptions'.
-- data RawPlot b v n where
--   RawPlot :: (InSpace v n a, Plotable a b) => a -> RawPlot b v n
--   deriving Typeable

-- type instance V (RawPlot b v n) = v
-- type instance N (RawPlot b v n) = n

-- instance (Metric v, OrderedField n) => Enveloped (RawPlot b v n) where
--   getEnvelope (RawPlot a) = getEnvelope a

-- instance (Metric v, OrderedField n, Typeable (RawPlot b v n))
--     => Plotable (RawPlot b v n) b where
--   renderPlotable s po sty (RawPlot p) = renderPlotable s po sty p
--   defLegendPic (RawPlot a) pp         = defLegendPic a pp

-- _RawPlot :: Plotable a b => Prism' (RawPlot b (V a) (N a)) a
-- _RawPlot = prism' RawPlot (\(RawPlot a) -> cast a)

------------------------------------------------------------------------
-- Plot modification
------------------------------------------------------------------------

-- | Modifications to 'PlotOptions' and 'PlotStyle' without being tied
--   to a specific plot.
data PlotMods b v n
  = PlotMods (PlotOptions b v n) (PlotStyle b v n -> PlotStyle b v n)

type instance V (PlotMods b v n) = v
type instance N (PlotMods b v n) = n

instance Functor f => HasPlotOptions f (PlotMods b v n) b where
  plotOptions f (PlotMods opts sty) = f opts <&> \opts' -> PlotMods opts' sty

instance Settable f => HasPlotStyle f (PlotMods b v n) b where
  plotStyle = sty . mapped where
    sty f (PlotMods opts s) = f s <&> \s' -> PlotMods opts s'

instance (Additive v, Num n) => Default (PlotMods b v n) where
  def = PlotMods def id

------------------------------------------------------------------------
-- Plot type
------------------------------------------------------------------------

-- | The main perpose of a 'PropertiedPlot' is so we can use lenses from
--   both the plot its self and the plots properites.
--
--   The 'PlotStyle' is not stored directly, because it isn't know until
--   the axis is rendered. Instead, the modification of the 'PlotStyle'
--   is used.
data Plot p b =
  Plot p
       (PlotOptions b (V p) (N p))
       (PlotStyle b (V p) (N p) -> PlotStyle b (V p) (N p))
  deriving Typeable

type instance V (Plot p b) = V p
type instance N (Plot p b) = N p

instance Functor f => HasPlotOptions f (Plot p b) b where
  plotOptions f (Plot p opts sty) = f opts <&> \opts' -> Plot p opts' sty

instance Settable f => HasPlotStyle f (Plot p b) b where
  plotStyle = sty . mapped where
    sty f (Plot p opts s) = f s <&> \s' -> Plot p opts s'

-- | Make a 'Plot' with 'Default' 'PlotOptions'.
mkPlot :: (Additive (V p), Num (N p)) => p -> Plot p b
mkPlot p = Plot p def id

-- | Lens onto the raw 'Plotable' inside a 'Plot'.
rawPlot :: SameSpace p p' => Lens (Plot p b) (Plot p' b) p p'
rawPlot f (Plot p opts ps) = f p <&> \p' -> Plot p' opts ps

-- | The modifications to the 'PlotOptions' and 'PlotStyle' in a 'Plot'.
plotMods :: Lens' (Plot p b) (PlotMods b (V p) (N p))
plotMods f (Plot p opts ps) =
  f (PlotMods opts ps) <&> \(PlotMods opts' ps') -> Plot p opts' ps'

------------------------------------------------------------------------
-- DynamicPlot
------------------------------------------------------------------------

-- | Internal type for storing plots in an axis.
data DynamicPlot b v n where
  DynamicPlot :: (InSpace v n p, Plotable p b) => Plot p b -> DynamicPlot b v n
  deriving Typeable

type instance V (DynamicPlot b v n) = v
type instance N (DynamicPlot b v n) = n

-- | Prism for a 'DynamicPlot'.
_DynamicPlot :: (Plotable p b, Typeable b) => Prism' (DynamicPlot b (V p) (N p)) (Plot p b)
_DynamicPlot = prism' DynamicPlot (\(DynamicPlot p) -> cast p)

instance Functor f => HasPlotOptions f (DynamicPlot b v n) b where
  plotOptions f (DynamicPlot (Plot p opts sty)) =
    f opts <&> \opts' -> DynamicPlot (Plot p opts' sty)

instance Settable f => HasPlotStyle f (DynamicPlot b v n) b where
  plotStyle = sty . mapped where
    sty :: Setter' (DynamicPlot b v n) (PlotStyle b v n -> PlotStyle b v n)
    sty f (DynamicPlot (Plot p opts s)) = f s <&> \s' -> DynamicPlot (Plot p opts s')

-- | The modifications to the 'PlotOptions' and 'PlotStyle' in a 'DynamicPlot'.
dynamicPlotMods :: Lens' (DynamicPlot b v n) (PlotMods b v n)
dynamicPlotMods f (DynamicPlot (Plot p opts ps)) =
  f (PlotMods opts ps) <&> \(PlotMods opts' ps') -> DynamicPlot (Plot p opts' ps')

------------------------------------------------------------------------
-- StyledPlot
------------------------------------------------------------------------

-- | A 'DynamicPlot' with a concrete style. This is suitable for being
--   rendered with 'styledPlotRender' and get extract the legend entries
--   with 'styledPlotLegend.
--
--   You can make a 'StyledPlot' with 'styleDynamic'
data StyledPlot b v n where
  StyledPlot
    :: Plotable p b
    => p
    -> PlotOptions b (V p) (N p)
    -> PlotStyle b (V p) (N p)
    -> StyledPlot b (V p) (N p)

type instance V (StyledPlot b v n) = v
type instance N (StyledPlot b v n) = n

instance Functor f => HasPlotOptions f (StyledPlot b v n) b where
  plotOptions f (StyledPlot p opts sty) =
    f opts <&> \opts' -> StyledPlot p opts' sty

instance (Metric v, OrderedField n) => Enveloped (StyledPlot b v n) where
  getEnvelope (StyledPlot p opts _) =
    getEnvelope p & transform (poTransform opts)

instance Functor f => HasPlotStyle f (StyledPlot b v n) b where
  plotStyle f (StyledPlot p opts sty) =
    f sty <&> StyledPlot p opts

-- | Give a 'DynamicPlot' a concrete 'PlotStyle'.
styleDynamic :: PlotStyle b v n -> DynamicPlot b v n -> StyledPlot b v n
styleDynamic sty (DynamicPlot (Plot p opts styF)) = StyledPlot p opts (styF sty)

-- | Render a 'StyledPlot' given an and 'AxisSpec'.
renderStyledPlot
  :: AxisSpec v n
  -> StyledPlot b v n
  -> QDiagram b v n Any
renderStyledPlot aSpec (StyledPlot p opts sty)
  = renderPlotable aSpec opts sty p

-- | Get the legend rendered entries from a single styled plot. The
--   resulting entries are in no particular order. See also
--   'styledPlotLegends'.
singleStyledPlotLegend
  :: StyledPlot b v n
  -> [(n, QDiagram b v n Any, String)] -- ^ @(z-order, legend pic, legend text)@
singleStyledPlotLegend (StyledPlot p opts sty) =
  map mk (opts ^. legendEntries)
  where
    mk entry = (entry ^. legendPrecedence, pic, entry ^. legendText)
      where
        pic = case lPic entry of
                DefaultLegendPic  -> defLegendPic p sty
                CustomLegendPic f -> f sty

-- | Render a list of legend entries, in order.
styledPlotLegends
  :: Ord n
  => [StyledPlot b v n]
  -> [(QDiagram b v n Any, String)] -- ^ @[(legend pic, legend text)]@
styledPlotLegends
  = map (\(_,p,t) -> (p,t))
  . sortOn (view _1)
  . concatMap singleStyledPlotLegend

-- instance (HasLinearMap (V p), Num (N p)) => Transformable (PropertiedPlot p b) where
--   transform = over plotTransform . transform

-- instance v ~ V p => HasBounds (PropertiedPlot p b) v where
--   bounds = plotBounds

-- type BaseV t = BaseSpace (V t)

-- -- | Move origin by applying to @plotTransform@.
-- instance (BaseV p ~ V p, Additive (V p), Num (N p)) => HasOrigin (PropertiedPlot p b) where
--   moveOriginTo = over plotTransform . moveOriginTo

-- instance BaseV p ~ V p => HasPlotStyle (PropertiedPlot p b) b where
--   plotStyle = plotPropertiesStyle

-- instance Qualifiable (PropertiedPlot p b) where
--   n .>> p = over plotName (n .>>) p

-- instance HasOrientation p => HasOrientation (PropertiedPlot p b) where
--   orientation = _pp . orientation

-- instance (Metric v, OrderedField n) => Enveloped (ModifiedPlot b v n) where
--   getEnvelope (ModifiedPlot p _) = getEnvelope p

