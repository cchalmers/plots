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
-- [@'PlotOptions' b v n@]
-- Generic options all plots have.
--
-- [@'PlotMods' b v n@]
-- Includes 'PlotOptions' along with modifications to the 'PlotStyle'.
--
-- [@'Plot' p b@]
-- A 'rawPlot' @p@ grouped with a 'PlotMods'.
--
-- [@'DynamicPlot' b v n@]
-- A wrapped up 'Plot' so it can be stored in an 'Axis'.
--
-- [@'StyledPlot' b v n@]
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
import           Diagrams.Prelude    as D

import           Plots.Axis.Scale
import           Plots.Style
import           Plots.Util

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
  gap :: Lens' a (N a)

-- | A 'Position' is a point on an axis together with an anchor and a
--   direction for the gap.
data Placement = Placement
  { pAt     :: V2 Rational
  , pAnchor :: V2 Rational
  , pGapDir :: Direction V2 Rational
  }
  deriving (Show, Read, Eq, Ord)
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
  gapDirection :: Lens' a (Direction V2 Rational)
  gapDirection = placement . lens pGapDir (\p a -> p {pGapDir = a})

instance HasPlacement Placement where
  placement = id

-- Inside positions ----------------------------------------------------

-- Internal helper for all inside placements
pInside :: V2 Rational -> Placement
pInside v = Placement
  { pAt     = v
  , pAnchor = v
  , pGapDir = dirBetween' (P v) origin
  }

-- | @dirBetween p q@ returns the directions from @p@ to @q@
dirBetween' :: (Additive v, Num n) => Point v n -> Point v n -> Direction v n
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
  :: (InSpace V2 n a, SameSpace a b, Enveloped a,
      HasOrigin b, Alignable b)
  => a -> Placement -> n -> b -> b
placeAgainst a (Placement (V2 px py) (V2 ax ay) d) n b
  = b # anchor
      # moveTo (pos .+^ n *^ fromDirection (fmap fromRational d))
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

-- | The order in which the legend entries are rendered. If precedences
--   are equal, the entries are put in the order they are added to the
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
  { poName      :: Name
  , poClipPlot  :: Bool
  , poLegend    :: [LegendEntry b v n]
  , poVisible   :: Bool
  , poTransform :: Transformation v n
  -- , poPostPlotBoundingBox :: BoundingBox v n -> BoundingBox v n
  -- , poPlotPostProduction  :: QDiagram b v n Any -> QDiagram b v n Any
  } deriving Typeable

type instance V (PlotOptions b v n) = v
type instance N (PlotOptions b v n) = n

-- | Class of things that have 'PlotOptions'.
class HasPlotOptions f a b | a -> b where
  {-# MINIMAL plotOptions #-}
  -- | Lens onto the 'PlotOptions'.
  plotOptions :: LensLike' f a (PlotOptions b (V a) (N a))

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
  legendEntries :: Functor f => LensLike' f a [LegendEntry b (V a) (N a)]
  legendEntries = plotOptions . lens poLegend (\g a -> g {poLegend = a})
  {-# INLINE legendEntries #-}

  -- | The transform applied to the plot once it's in the axis
  --   coordinates.
  --
  --   'Default' is 'mempty'.
  plotTransform :: Functor f => LensLike' f a (Transformation (V a) (N a))
  plotTransform = plotOptions . lens poTransform (\g a -> g {poTransform = a})
  {-# INLINE plotTransform #-}

  -- | Whether or not the plot should be shown. The 'BoundingBox' of the
  --   plot will still affect the inferred axis bounds.
  --
  --   'Default' is 'True'.
  plotVisible :: Functor f => LensLike' f a Bool
  plotVisible = plotOptions . lens poVisible (\po b -> po {poVisible = b})
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

instance Qualifiable (PlotOptions b v n) where
  n .>> p = over plotName (n .>>) p

-- XXX template haskell getting in the way
-- instance HasVisibility (PlotOptions b v n) where
--   visible = plotVisible

-- | Add a 'LegendEntry' to something with 'PlotOptions' using the
--   'String' as the 'legendText' and a 'DefaultLegendPic'. Here are
--   some typical examples:
--
-- @
-- 'key' :: 'String' -> 'State' ('Plot' ('ScatterPlot' v n) b) ()
-- 'key' :: 'String' -> 'State' ('DynamicPlot' b v n) ()
-- 'key' :: 'String' -> 'State' ('PlotMods' b v n) ()
-- @
--
--  If you only care about the name of the legend, use 'key'.
key :: (HasPlotOptions Identity a b, MonadState a m, Num (N a)) => String -> m ()
key = addLegendEntry . mkLegendEntry

-- | Add a 'LegendEntry' to something with 'PlotOptions'. Here are some
--   typical examples:
--
-- @
-- 'addLegendEntry' :: 'LegendEntry' b v n -> 'State' ('Plot' ('ScatterPlot' v n) b) ()
-- 'addLegendEntry' :: 'LegendEntry' b v n -> 'State' ('DynamicPlot' b v n) ()
-- @
--
--  If you only care about the name of the legend, use 'key'.
addLegendEntry
  :: (HasPlotOptions Identity a b, MonadState a m)
  => LegendEntry b (V a) (N a)
  -> m ()
addLegendEntry l = legendEntries <>= [l]

-- zeroInt :: Additive v => v Int
-- zeroInt = zero

------------------------------------------------------------------------
-- AxisSpec
------------------------------------------------------------------------

-- | Information from the 'Plots.Axis.Axis' necessary to render a 'Plotable'.
data AxisSpec v n = AxisSpec
  { _specBounds    :: v (n, n)
  , _specTrans     :: Transformation v n
  , _specScale     :: v LogScale
  , _specColourMap :: ColourMap
  }

makeLenses ''AxisSpec

type instance V (AxisSpec v n) = v
type instance N (AxisSpec v n) = n

-- | Scale a number by log10-ing it and linearly scaling it so it's
--   within the same range.
scaleNum :: Floating n => (n, n) -> LogScale -> n -> n
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

-- | Class defining how plots should be rendered.
class (Typeable p, Enveloped p) => Plotable p b where
  -- | Render a plot according to the 'AxisSpec', using the 'PlotStyle'.
  renderPlotable
    :: InSpace v n p
    => AxisSpec v n
    -> PlotStyle b v n
    -> p
    -> QDiagram b v n Any

  -- | The default legend picture when the 'LegendPic' is
  --   'DefaultLegendPic'.
  defLegendPic
    :: InSpace v n p
    => PlotStyle b v n
    -> p
    -> QDiagram b v n Any
  defLegendPic = mempty

instance (Typeable b, Typeable v, Metric v, Typeable n, OrderedField n)
  => Plotable (QDiagram b v n Any) b where
  renderPlotable s _ dia = dia # transform (s^.specTrans)

instance (TypeableFloat n, Renderable (Path V2 n) b) => Plotable (Path V2 n) b where
  renderPlotable s sty path
    = stroke path
        # transform (s^.specTrans)
        # applyLineStyle sty

  defLegendPic sty _
    = (p2 (-10,0) ~~ p2 (10,0))
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

instance HasVisibility (PlotOptions b v n) where
  visible = plotVisible

instance HasVisibility (PlotMods b v n) where
  visible = plotVisible

instance HasVisibility (Plot p b) where
  visible = plotVisible

instance HasVisibility (DynamicPlot b v n) where
  visible = plotVisible

instance HasVisibility (StyledPlot b v n) where
  visible = plotVisible

-- | Set 'visible' to 'False' for the given setter.
--
-- @
-- 'hide' 'minorTicks'          :: 'State' ('Axis' b v n) ()
-- 'hide' ('xAxis' . 'gridLines') :: 'State' ('Axis' b v n) ()
-- @
hide :: (MonadState s m, HasVisibility a) => ASetter' s a -> m ()
hide l = l . visible .= False

-- | Set 'visible' to 'True' for the given setter.
--
-- @
-- 'display' 'minorGridLines' :: 'State' ('Axis' b v n) ()
-- 'display' 'colourBar'      :: 'State' ('Axis' b v n) ()
-- @
display :: (MonadState s m, HasVisibility a) => ASetter' s a -> m ()
display l = l . visible .= True

------------------------------------------------------------------------
-- Plot modification
------------------------------------------------------------------------

-- | A 'PlotOptions' with modifications to a 'PlotStyle'.
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

-- | A parameterised plot, together with a 'PlotMods'. This type has an
--   instance of many classes for modifying specific plots.
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

instance HasOrientation p => HasOrientation (Plot p b) where
  orientation = rawPlot . orientation

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

-- | A wrapped up 'Plot', used to store plots in an 'Axis'.
data DynamicPlot b v n where
  DynamicPlot :: (InSpace v n p, Plotable p b) => Plot p b -> DynamicPlot b v n
  deriving Typeable

type instance V (DynamicPlot b v n) = v
type instance N (DynamicPlot b v n) = n

-- | Prism for a 'DynamicPlot'.
_DynamicPlot :: (Plotable p b, Typeable b) => Prism' (DynamicPlot b (V p) (N p)) (Plot p b)
_DynamicPlot = prism' DynamicPlot (\(DynamicPlot p) -> cast p)

-- | Traversal over the dynamic plot without the 'Plotable' constraint
--   '_DynamicPlot' has.
dynamicPlot :: forall p b. (Typeable p, Typeable b)
            => Traversal' (DynamicPlot b (V p) (N p)) (Plot p b)
dynamicPlot f d@(DynamicPlot p) =
  case eq p of
    Just Refl -> f p <&> \p' -> DynamicPlot p'
    Nothing   -> pure d
  where eq :: Typeable a => a -> Maybe (a :~: Plot p b)
        eq _ = eqT
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
--   rendered with 'renderStyledPlot' and get extract the legend entries
--   with 'styledPlotLegend'.
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

-- | Traversal over a raw plot of a styled plot. The type of the plot
--   must match for the traversal to be successful.
styledPlot :: forall p b. Typeable p => Traversal' (StyledPlot b (V p) (N p)) p
styledPlot f s@(StyledPlot p opts sty) =
  case eq p of
    Just Refl -> f p <&> \p' -> StyledPlot p' opts sty
    Nothing   -> pure s
  where eq :: Typeable a => a -> Maybe (a :~: p)
        eq _ = eqT

-- | Give a 'DynamicPlot' a concrete 'PlotStyle'.
styleDynamic :: PlotStyle b v n -> DynamicPlot b v n -> StyledPlot b v n
styleDynamic sty (DynamicPlot (Plot p opts styF)) = StyledPlot p opts (styF sty)

-- | Render a 'StyledPlot' given an and 'AxisSpec'.
renderStyledPlot
  :: TypeableFloat n
  => AxisSpec V2 n
  -> StyledPlot b V2 n
  -> QDiagram b V2 n Any
renderStyledPlot aSpec (StyledPlot p opts sty)
  = renderPlotable aSpec sty p
      & whenever (opts^.hidden) phantom
      & whenever (opts^.clipPlot) (clipTo $ specRect aSpec)

specRect :: TypeableFloat n => AxisSpec V2 n -> Path V2 n
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
singleStyledPlotLegend
  :: StyledPlot b v n
  -> [(n, QDiagram b v n Any, String)] -- ^ @(z-order, legend pic, legend text)@
singleStyledPlotLegend (StyledPlot p opts sty) =
  map mk (opts ^. legendEntries)
  where
    mk entry = (entry ^. legendPrecedence, pic, entry ^. legendText)
      where
        pic = case lPic entry of
                DefaultLegendPic  -> defLegendPic sty p
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

-- XXX taken from "Data.List", defined here because it was only added in
-- base-4.8 (ghc-7.10)
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f =
  map snd . sortBy (comparing fst) . map (\x -> let y = f x in y `seq` (y, x))

