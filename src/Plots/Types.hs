{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Plots.Types
-- Copyright   :  (C) 2015 Christopher Chalmers
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Christopher Chalmers
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module defines the various types for holding plots:
--
-- [@'PlotOptions' v@]
-- Generic options all plots have.
--
-- [@'PlotMods' v@]
-- Includes 'PlotOptions' along with modifications to the 'PlotStyle'.
--
-- [@'Plot' p@]
-- A 'rawPlot' @p@ grouped with a 'PlotMods'.
--
-- [@'DynamicPlot' v@]
-- A wrapped up 'Plot' so it can be stored in an 'Axis'.
--
-- [@'StyledPlot' v@]
-- A 'DynamicPlot' with a concrete 'PlotStyle', ready to be rendered.
--
-- As well as other things like the 'Plotable' class, 'LegendEntries',
-- 'HasOrientation' and 'HasVisibility'.
--
----------------------------------------------------------------------------
module Plots.Types
  (

    -- * Plot options
    PlotOptions
  , HasPlotOptions (..)
  , key
  , addLegendEntry

    -- ** Plot modifications
  , PlotMods
  , plotMods

    -- * Plotable class
  , Plotable (..)

    -- * Plot types
    -- ** Parameterised plot
  , Plot
  , mkPlot
  , rawPlot

    -- ** Dynamic plot
  , DynamicPlot (..)
  , _DynamicPlot
  , dynamicPlot
  , dynamicPlotMods

    -- ** Styled plot
  , StyledPlot
  , styledPlot
  , styleDynamic
  , renderStyledPlot
  , singleStyledPlotLegend
  , styledPlotLegends

  -- * Miscellaneous
  -- ** Visibility
  , HasVisibility (..)
  , hide
  , display

  -- ** Orientation
  , Orientation (..)
  , HasOrientation (..)
  , orient
  , horizontal
  , vertical

  -- ** Legend entries
  , LegendEntry
  , LegendPic (..)
  , mkLegendEntry
  , legendPicture
  , legendText
  , legendPrecedence

    -- ** Axis spec
  , AxisSpec (..)
  , specTrans
  , specBounds
  , specScale
  , scaleNum
  , specPoint
  , specColourMap

  -- ** Positioning
  , Placement (..)
  , HasPlacement (..)
  , HasGap (..)
  , placeAgainst

  -- *** Common positions
  -- **** Inside positions
  , topLeft, top, topRight, left, right, bottomLeft, bottom
  , bottomRight

  -- **** Outside positions
  , leftAbove, leftTop, leftMid, leftBottom, leftBelow, midAbove, midBelow
  , rightAbove, rightTop, rightMid, rightBottom, rightBelow

  ) where

import           Control.Monad.State
import           Data.Bool
import           Data.List           (sortBy)
import           Data.Maybe          (fromMaybe)
import           Data.Ord            (comparing)
import           Data.Orphans        ()
import           Data.Typeable
import           Diagrams.Prelude    as D hiding (orient)

import           Plots.Axis.Scale
import           Plots.Style
import           Plots.Util

-- XXX shouldn't need to import these
import Diagrams.Combinators as D
import Geometry.TwoD.Shapes

import Diagrams.Types

-- Orientation ---------------------------------------------------------

data Orientation = Horizontal | Vertical
  deriving (Show, Eq, Ord, Typeable)

-- | Pick the first @a@ if the object has 'Horizontal' orientation and
--   the second @a@ if the object has a 'Vertical' orientation.
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
-- Placement
------------------------------------------------------------------------

class HasGap a where
  -- | The value of the gap when rendering.
  gap :: Lens' a Double

-- | A 'Position' is a point on an axis together with an anchor and a
--   direction for the gap.
data Placement = Placement
  { pAt     :: V2 Rational
  , pAnchor :: V2 Rational
  , pGapDir :: Direction V2 Double
  }
  deriving (Show, Eq)
  -- In the future the axis position may be replaced by a reader-like
  -- anchor system where you can choose parts of the rendered axis as a
  -- position.
  -- we keep posGap and posGapDir as separate values to keep lens laws
  -- for 0 gaps
  -- I'm not sure this should work for a 3D axis

class HasPlacement a where
  placement :: Lens' a Placement

  -- | The position relative to the axis. @V2 0 0@ corresponds to the
  --   bottom left corner, @V2 1 1@ is the top right corner.
  placementAt :: Lens' a (V2 Rational)
  placementAt = placement . lens pAt (\p a -> p {pAt = a})

  -- | The anchor used for the object being positioned. @V2 0 0@
  --   corresponds to the bottom left corner, @V2 1 1@ is the top right
  --   corner.
  placementAnchor :: Lens' a (V2 Rational)
  placementAnchor = placement . lens pAnchor (\p a -> p {pAnchor = a})

  -- | The direction to extend the 'gap' when positioning.
  gapDirection :: Lens' a (Direction V2 Double)
  gapDirection = placement . lens pGapDir (\p a -> p {pGapDir = a})

instance HasPlacement Placement where
  placement = id

-- Inside positions ----------------------------------------------------

-- Internal helper for all inside placements
pInside :: V2 Rational -> Placement
pInside v = Placement
  { pAt     = v
  , pAnchor = v
  , pGapDir = dirBetween' (P $ fromRational <$> v) origin
  }

-- | @dirBetween p q@ returns the directions from @p@ to @q@
dirBetween' :: (Metric v, Floating n) => Point v n -> Point v n -> Direction v n
dirBetween' p q = direction $ q .-. p


topLeft, top, topRight, left, right, bottomLeft, bottom, bottomRight :: Placement
topLeft     = pInside (V2 (-1)   1 )
top         = pInside (V2   0    1 )
topRight    = pInside (V2   1    1 )
left        = pInside (V2 (-1)   0 )
right       = pInside (V2 (-1)   0 )
bottomLeft  = pInside (V2 (-1) (-1))
bottom      = pInside (V2   0  (-1))
bottomRight = pInside (V2   1  (-1))

-- Outside positions ---------------------------------------------------

leftAbove, leftTop, leftMid, leftBottom, leftBelow, midAbove, midBelow,
  rightAbove, rightTop, rightMid, rightBottom, rightBelow :: Placement

leftAbove   = Placement (V2 (-1)   1 ) (V2 (-1) (-1)) (direction (V2   0    1 ))
leftTop     = Placement (V2 (-1)   1 ) (V2   1    1 ) (direction (V2 (-1)   0 ))
leftMid     = Placement (V2 (-1)   0 ) (V2   1    0 ) (direction (V2 (-1)   0 ))
leftBottom  = Placement (V2 (-1) (-1)) (V2   1  (-1)) (direction (V2 (-1)   0 ))
leftBelow   = Placement (V2 (-1) (-1)) (V2 (-1)   1 ) (direction (V2   0  (-1)))

midAbove    = Placement (V2   0    1 ) (V2   0  (-1)) (direction (V2   0    1 ))
midBelow    = Placement (V2   0  (-1)) (V2   0    1 ) (direction (V2   0  (-1)))

rightAbove  = Placement (V2   1    1 ) (V2   1  (-1)) (direction (V2   0    1 ))
rightTop    = Placement (V2   1    1 ) (V2 (-1)   1 ) (direction (V2   1    0 ))
rightMid    = Placement (V2   1    0 ) (V2 (-1)   0 ) (direction (V2   1    0 ))
rightBottom = Placement (V2   1  (-1)) (V2 (-1) (-1)) (direction (V2   1    0 ))
rightBelow  = Placement (V2   1  (-1)) (V2   1    1 ) (direction (V2   0  (-1)))

-- Using positions -----------------------------------------------------

-- | A tool for aligned one object to another. Positions @b@ around the
--   bounding box of @a@ by translating @b@.
placeAgainst
  :: (InSpace V2 n a, SameSpace a b, Enveloped a, Enveloped b, HasOrigin b)
  => a -> Placement -> n -> b -> b
placeAgainst a (Placement (V2 px py) (V2 ax ay) d) n b
  = b # anchor
      # moveTo (pos .+^ n *^ fromDirection (realToFrac <$> d))
  where
    pos    = mkP2 (lerp' px xu xl) (lerp' py yu yl)
    anchor = alignBy unitX (fromRational ax) . alignBy unitY (fromRational ay)
    (P (V2 xl yl), P (V2 xu yu)) = fromMaybe (origin, origin) (getCorners $ boundingBox a)

    lerp' z u v = fromRational alpha * u + (1 - fromRational alpha) * v
      where alpha = (z + 1) / 2

------------------------------------------------------------------------
-- Legend entries
------------------------------------------------------------------------

-- | Type allowing use of the default legend picture (depending on the
--   plot) or a custom legend picture with access to the 'PlotStyle'.
data LegendPic v
  = DefaultLegendPic
  | CustomLegendPic (PlotStyle v -> Diagram v)

instance Default (LegendPic v) where
  def = DefaultLegendPic

-- | Data type for holding a legend entry.
data LegendEntry v = LegendEntry
  { lPic        :: LegendPic v
  , lText       :: String
  , lPrecedence :: Double
  } deriving Typeable

-- | The picture used in the legend entry.
legendPicture :: Lens' (LegendEntry v) (LegendPic v)
legendPicture = lens lPic (\l pic -> l {lPic = pic})

-- | The text used in the legend entry.
legendText :: Lens' (LegendEntry v) String
legendText = lens lText (\l txt -> l {lText = txt})

-- | The order in which the legend entries are rendered. If precedences
--   are equal, the entries are put in the order they are added to the
--   axis.
--
--   Default is @0@.
legendPrecedence :: Lens' (LegendEntry v) Double
legendPrecedence = lens lPrecedence (\l n -> l {lPrecedence = n})

type instance V (LegendEntry v) = v
type instance N (LegendEntry v) = Double

-- | Make a legend entry with a default 'legendPicture' and
--   'legendPrecedence' 0 using the string as the 'legendText'.
mkLegendEntry :: String -> LegendEntry v
mkLegendEntry x = LegendEntry DefaultLegendPic x 0

------------------------------------------------------------------------
-- Plot attributes
------------------------------------------------------------------------

-- Generic Plot info

-- | Data type for holding information all plots must contain.
data PlotOptions v = PlotOptions
  { poName      :: Name
  , poClipPlot  :: Bool
  , poLegend    :: [LegendEntry v]
  , poVisible   :: Bool
  , poTransform :: Transformation v Double
  -- , poPostPlotBoundingBox :: BoundingBox v n -> BoundingBox v n
  -- , poPlotPostProduction  :: QDiagram v Any -> QDiagram v Any
  } deriving Typeable

type instance V (PlotOptions v) = v
type instance N (PlotOptions v) = Double

-- | Class of things that have 'PlotOptions'.
class HasPlotOptions f a where
  {-# MINIMAL plotOptions #-}
  -- | Lens onto the 'PlotOptions'.
  plotOptions :: LensLike' f a (PlotOptions (V a))

  -- | The 'Name' applied to the plot. This gives a way to reference a
  --   specific plot in a rendered axis.
  --
  --   'Default' is 'mempty'.
  plotName :: Functor f => LensLike' f a Name
  plotName = plotOptions . lens poName (\g a -> g {poName = a})
  {-# INLINE plotName #-}

  -- | Whether the plot should be clipped to the bounds of the axes.
  --
  --   'Default' is 'True'.
  clipPlot :: Functor f => LensLike' f a Bool
  clipPlot = plotOptions . lens poClipPlot (\g a -> g {poClipPlot = a})
  {-# INLINE clipPlot #-}

  -- | The legend entries to be used for the current plot.
  --
  --   'Default' is 'mempty'.
  legendEntries :: Functor f => LensLike' f a [LegendEntry (V a)]
  legendEntries = plotOptions . lens poLegend (\g a -> g {poLegend = a})
  {-# INLINE legendEntries #-}

  -- | The transform applied to the plot once it's in the axis
  --   coordinates.
  --
  --   'Default' is 'mempty'.
  plotTransform :: Functor f => LensLike' f a (Transformation (V a) Double)
  plotTransform = plotOptions . lens poTransform (\g a -> g {poTransform = a})
  {-# INLINE plotTransform #-}

  -- | Whether or not the plot should be shown. The 'BoundingBox' of the
  --   plot will still affect the inferred axis bounds.
  --
  --   'Default' is 'True'.
  plotVisible :: Functor f => LensLike' f a Bool
  plotVisible = plotOptions . lens poVisible (\po b -> po {poVisible = b})
  {-# INLINE plotVisible #-}

instance (HasBasis v, Foldable v) => Default (PlotOptions v) where
  def = PlotOptions
    { poName                = mempty
    , poClipPlot            = True
    , poLegend              = []
    , poVisible             = True
    , poTransform           = mempty
    -- , poPostPlotBoundingBox = id
    -- , poPlotPostProduction  = id
    }

instance HasPlotOptions f (PlotOptions v) where
  plotOptions = id
  {-# INLINE plotOptions #-}

instance HasLinearMap v => Transformable (PlotOptions v) where
  transform = over plotTransform . transform

-- instance HasBounds (PlotOptions v) v where
--   bounds = plotBounds

-- | Move origin by applying to @plotTransform@.
instance Additive v => HasOrigin (PlotOptions v) where
  moveOriginTo = over plotTransform . moveOriginTo

instance Qualifiable (PlotOptions v) where
  n .>> p = over plotName (n .>>) p

-- XXX template haskell getting in the way
-- instance HasVisibility (PlotOptions v) where
--   visible = plotVisible

-- | Add a 'LegendEntry' to something with 'PlotOptions' using the
--   'String' as the 'legendText' and a 'DefaultLegendPic'. Here are
--   some typical examples:
--
-- @
-- 'key' :: 'String' -> 'State' ('Plot' ('ScatterPlot' v)) ()
-- 'key' :: 'String' -> 'State' ('DynamicPlot' v) ()
-- 'key' :: 'String' -> 'State' ('PlotMods' v) ()
-- @
--
--  If you only care about the name of the legend, use 'key'.
key :: (HasPlotOptions Identity a, MonadState a m) => String -> m ()
key = addLegendEntry . mkLegendEntry

-- | Add a 'LegendEntry' to something with 'PlotOptions'. Here are some
--   typical examples:
--
-- @
-- 'addLegendEntry' :: 'LegendEntry' v -> 'State' ('Plot' ('ScatterPlot' v)) ()
-- 'addLegendEntry' :: 'LegendEntry' v -> 'State' ('DynamicPlot' v) ()
-- @
--
--  If you only care about the name of the legend, use 'key'.
addLegendEntry
  :: (HasPlotOptions Identity a, MonadState a m)
  => LegendEntry (V a)
  -> m ()
addLegendEntry l = legendEntries <>= [l]

-- zeroInt :: Additive v => v Int
-- zeroInt = zero

------------------------------------------------------------------------
-- AxisSpec
------------------------------------------------------------------------

-- | Information from the 'Plots.Axis.Axis' necessary to render a 'Plotable'.
data AxisSpec v = AxisSpec
  { _specBounds    :: v (Double, Double)
  , _specTrans     :: Transformation v Double
  , _specScale     :: v LogScale
  , _specColourMap :: ColourMap
  }

makeLenses ''AxisSpec

type instance V (AxisSpec v) = v
type instance N (AxisSpec v) = Double

-- | Scale a number by log10-ing it and linearly scaling it so it's
--   within the same range.
scaleNum :: Floating n => (n, n) -> LogScale -> n -> n
scaleNum (a,b) s x = case s of
  LinearAxis -> x
  LogAxis    -> subtract a $ (b / logBase 10 d) * (logBase 10 x)
    where d = b - a

-- | Apply log scaling and the transform to a point.
specPoint
  :: (Applicative v, Additive v, Foldable v)
  => AxisSpec v -> Point v Double -> Point v Double
specPoint (AxisSpec bs tr ss _) p =
  papply tr $ over _Point (scaleNum <$> bs <*> ss <*>) p

------------------------------------------------------------------------
-- Plotable class
------------------------------------------------------------------------

-- | Class defining how plots should be rendered.
class (Typeable p, Enveloped p, N p ~ Double) => Plotable p where
  -- | Render a plot according to the 'AxisSpec', using the 'PlotStyle'.
  renderPlotable
    :: InSpace v Double p
    => AxisSpec v
    -> PlotStyle v
    -> p
    -> Diagram v

  -- | The default legend picture when the 'LegendPic' is
  --   'DefaultLegendPic'.
  defLegendPic
    :: InSpace v Double p
    => PlotStyle v
    -> p
    -> Diagram v
  defLegendPic = mempty

instance (Typeable v, HasLinearMap v) => Plotable (Diagram v) where
  renderPlotable s _ dia = dia # transform (s^.specTrans)

strokePathV :: (Typeable v, Metric v, Typeable n, OrderedField n) => Path v n -> QDiagram v n Any
strokePathV path = mkQD (Prim path) (getEnvelope path) mempty mempty

instance (Typeable v, R1 v, HasLinearMap v) => Plotable (Path v Double) where
  renderPlotable s sty path
    = strokePathV path
        # transform (s^.specTrans)
        # applyLineStyle sty

  defLegendPic sty _
    = strokePathV (fromVertices [(-10) *^ unitX, 10 *^ unitX])
        # applyLineStyle sty

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

instance HasVisibility (PlotOptions v) where
  visible = plotVisible

instance HasVisibility (PlotMods v) where
  visible = plotVisible

instance HasVisibility (Plot p) where
  visible = plotVisible

instance HasVisibility (DynamicPlot v) where
  visible = plotVisible

instance HasVisibility (StyledPlot v) where
  visible = plotVisible

-- | Set 'visible' to 'False' for the given setter.
--
-- @
-- 'hide' 'minorTicks'          :: 'State' ('Axis' v) ()
-- 'hide' ('xAxis' . 'gridLines') :: 'State' ('Axis' v) ()
-- @
hide :: (MonadState s m, HasVisibility a) => ASetter' s a -> m ()
hide l = l . visible .= False

-- | Set 'visible' to 'True' for the given setter.
--
-- @
-- 'display' 'minorGridLines' :: 'State' ('Axis' v) ()
-- 'display' 'colourBar'      :: 'State' ('Axis' v) ()
-- @
display :: (MonadState s m, HasVisibility a) => ASetter' s a -> m ()
display l = l . visible .= True

------------------------------------------------------------------------
-- Plot modification
------------------------------------------------------------------------

-- | A 'PlotOptions' with modifications to a 'PlotStyle'.
data PlotMods v
  = PlotMods (PlotOptions v) (PlotStyle v -> PlotStyle v)

type instance V (PlotMods v) = v
type instance N (PlotMods v) = Double

instance Functor f => HasPlotOptions f (PlotMods v) where
  plotOptions f (PlotMods opts sty) = f opts <&> \opts' -> PlotMods opts' sty

instance Settable f => HasPlotStyle f (PlotMods v) where
  plotStyle = sty . mapped where
    sty f (PlotMods opts s) = f s <&> \s' -> PlotMods opts s'

instance (HasBasis v, Foldable v) => Default (PlotMods v) where
  def = PlotMods def id

------------------------------------------------------------------------
-- Plot type
------------------------------------------------------------------------

-- | A parameterised plot, together with a 'PlotMods'. This type has an
--   instance of many classes for modifying specific plots.
data Plot p =
  Plot p
       (PlotOptions (V p))
       (PlotStyle (V p) -> PlotStyle (V p))
  deriving Typeable

type instance V (Plot p) = V p
type instance N (Plot p) = Double

instance Functor f => HasPlotOptions f (Plot p) where
  plotOptions f (Plot p opts sty) = f opts <&> \opts' -> Plot p opts' sty

instance Settable f => HasPlotStyle f (Plot p) where
  plotStyle = sty . mapped where
    sty f (Plot p opts s) = f s <&> \s' -> Plot p opts s'

instance HasOrientation p => HasOrientation (Plot p) where
  orientation = rawPlot . orientation

-- | Make a 'Plot' with 'Default' 'PlotOptions'.
mkPlot :: (InSpace v Double p, HasBasis v, Foldable v) => p -> Plot p
mkPlot p = Plot p def id

-- | Lens onto the raw 'Plotable' inside a 'Plot'.
rawPlot :: SameSpace p p' => Lens (Plot p) (Plot p') p p'
rawPlot f (Plot p opts ps) = f p <&> \p' -> Plot p' opts ps

-- | The modifications to the 'PlotOptions' and 'PlotStyle' in a 'Plot'.
plotMods :: Lens' (Plot p) (PlotMods (V p))
plotMods f (Plot p opts ps) =
  f (PlotMods opts ps) <&> \(PlotMods opts' ps') -> Plot p opts' ps'

------------------------------------------------------------------------
-- DynamicPlot
------------------------------------------------------------------------

-- | A wrapped up 'Plot', used to store plots in an 'Axis'.
data DynamicPlot v where
  DynamicPlot :: (InSpace v Double p, Plotable p) => Plot p -> DynamicPlot v
  deriving Typeable

type instance V (DynamicPlot v) = v
type instance N (DynamicPlot v) = Double

-- | Prism for a 'DynamicPlot'.
_DynamicPlot :: Plotable p => Prism' (DynamicPlot (V p)) (Plot p)
_DynamicPlot = prism' DynamicPlot (\(DynamicPlot p) -> cast p)

-- | Traversal over the dynamic plot without the 'Plotable' constraint
--   '_DynamicPlot' has.
dynamicPlot :: forall p. Typeable p => Traversal' (DynamicPlot (V p)) (Plot p)
dynamicPlot f d@(DynamicPlot p) =
  case eq p of
    Just Refl -> f p <&> \p' -> DynamicPlot p'
    Nothing   -> pure d
  where eq :: Typeable a => a -> Maybe (a :~: Plot p)
        eq _ = eqT

instance Functor f => HasPlotOptions f (DynamicPlot v) where
  plotOptions f (DynamicPlot (Plot p opts sty)) =
    f opts <&> \opts' -> DynamicPlot (Plot p opts' sty)

instance Settable f => HasPlotStyle f (DynamicPlot v) where
  plotStyle = sty . mapped where
    sty :: Setter' (DynamicPlot v) (PlotStyle v -> PlotStyle v)
    sty f (DynamicPlot (Plot p opts s)) = f s <&> \s' -> DynamicPlot (Plot p opts s')

-- | The modifications to the 'PlotOptions' and 'PlotStyle' in a 'DynamicPlot'.
dynamicPlotMods :: Lens' (DynamicPlot v) (PlotMods v)
dynamicPlotMods f (DynamicPlot (Plot p opts ps)) =
  f (PlotMods opts ps) <&> \(PlotMods opts' ps') -> DynamicPlot (Plot p opts' ps')

------------------------------------------------------------------------
-- StyledPlot
------------------------------------------------------------------------

-- | A 'DynamicPlot' with a concrete style. This is suitable for being
--   rendered with 'renderStyledPlot' and get extract the legend entries
--   with 'styledPlotLegend'.
--
--   You can make a 'StyledPlot' with 'styleDynamic'
data StyledPlot v where
  StyledPlot
    :: Plotable p
    => p
    -> PlotOptions (V p)
    -> PlotStyle (V p)
    -> StyledPlot (V p)

type instance V (StyledPlot v) = v
type instance N (StyledPlot v) = Double

instance Functor f => HasPlotOptions f (StyledPlot v) where
  plotOptions f (StyledPlot p opts sty) =
    f opts <&> \opts' -> StyledPlot p opts' sty

instance HasLinearMap v => Enveloped (StyledPlot v) where
  getEnvelope (StyledPlot p opts _) =
    getEnvelope p & transform (poTransform opts)

instance Functor f => HasPlotStyle f (StyledPlot v) where
  plotStyle f (StyledPlot p opts sty) =
    f sty <&> StyledPlot p opts

-- | Traversal over a raw plot of a styled plot. The type of the plot
--   must match for the traversal to be successful.
styledPlot :: forall p. Typeable p => Traversal' (StyledPlot (V p)) p
styledPlot f s@(StyledPlot p opts sty) =
  case eq p of
    Just Refl -> f p <&> \p' -> StyledPlot p' opts sty
    Nothing   -> pure s
  where eq :: Typeable a => a -> Maybe (a :~: p)
        eq _ = eqT

-- | Give a 'DynamicPlot' a concrete 'PlotStyle'.
styleDynamic :: PlotStyle v -> DynamicPlot v -> StyledPlot v
styleDynamic sty (DynamicPlot (Plot p opts styF)) = StyledPlot p opts (styF sty)

-- | Render a 'StyledPlot' given an and 'AxisSpec'.
renderStyledPlot :: HasLinearMap v => AxisSpec v -> StyledPlot v -> Diagram v
renderStyledPlot aSpec (StyledPlot p opts sty)
  = renderPlotable aSpec sty p
      & whenever (opts^.hidden) D.phantom
      -- & whenever (opts^.clipPlot) (clipTo $ specRect aSpec) XXX ADD CLIPTO XXX

specRect :: AxisSpec V2 -> Path V2 Double
specRect aSpec =
  rect (xU - xL) (yU - yL)
    # moveTo (mkP2 ((xU+xL)/2) ((yU+yL)/2))
    # transform t
  where
  V2 (xL,xU) (yL,yU) = _specBounds aSpec
  t                   = _specTrans aSpec

-- | Get the legend rendered entries from a single styled plot. The
--   resulting entries are in no particular order. See also
--   'styledPlotLegends'.
singleStyledPlotLegend :: StyledPlot v -> [(Double, Diagram v, String)] -- ^ @(z-order, legend pic, legend text)@
singleStyledPlotLegend (StyledPlot p opts sty) =
  map mk (opts ^. legendEntries)
  where
    mk entry = (entry ^. legendPrecedence, pic, entry ^. legendText)
      where
        pic = case lPic entry of
                DefaultLegendPic  -> defLegendPic sty p
                CustomLegendPic f -> f sty

-- | Render a list of legend entries, in order.
styledPlotLegends :: [StyledPlot v] -> [(Diagram v, String)] -- ^ @[(legend pic, legend text)]@
styledPlotLegends
  = map (\(_,p,t) -> (p,t))
  . sortOn (view _1)
  . concatMap singleStyledPlotLegend

-- XXX taken from "Data.List", defined here because it was only added in
-- base-4.8 (ghc-7.10)
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f =
  map snd . sortBy (comparing fst) . map (\x -> let y = f x in y `seq` (y, x))

