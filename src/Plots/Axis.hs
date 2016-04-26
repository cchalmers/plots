{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Plots.Axis
-- Copyright   :  (C) 2015 Christopher Chalmers
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Christopher Chalmers
-- Stability   :  experimental
-- Portability :  non-portable
--
-- The 'Axis' is the main data type for "plots". It holds all the
-- nessesary infomation to be rendered into a 'Diagram'.
--
----------------------------------------------------------------------------
module Plots.Axis
  ( -- * Axis type
    Axis
  , axes
  , axisPlots
  , currentPlots
  , finalPlots
  , plotModifier
  , axisSize

    -- * Predefined axes
  , r2Axis

    -- ** Base space
  , BaseSpace

    -- * Axis plots
  , addPlot
  , addPlotable
  , addPlotable'

    -- * Single axis
  , SingleAxis

    -- * Specific axes
    -- ** x-axis
  , xAxis
  , xLabel
  , xMin
  , xMax

    -- ** y-axis
  , yAxis
  , yLabel
  , yMin
  , yMax

    -- ** z-axis
  , zAxis
  , zLabel
  , zMin
  , zMax
  ) where

import           Control.Monad.State
import           Data.Complex
import           Data.Default
import           Data.Typeable

import           Diagrams.Coordinates.Polar
import           Diagrams.Prelude
import           Diagrams.TwoD.Text

import           Plots.Axis.ColourBar
import           Plots.Axis.Grid
import           Plots.Axis.Labels
import           Plots.Axis.Line
import           Plots.Axis.Scale
import           Plots.Axis.Ticks
import           Plots.Legend
import           Plots.Style
import           Plots.Types

import           Linear

------------------------------------------------------------------------
-- Axis data type
------------------------------------------------------------------------

-- Single axis ---------------------------------------------------------

-- | Render infomation for a single axis line.
data SingleAxis b v n = SingleAxis
-- note the the v is only present for Style v n
  { saLabel     :: AxisLabel b v n
  , saLine      :: AxisLine v n
  , saTickLabel :: TickLabels b v n
  , saScaling   :: AxisScaling n
  , saGridLines :: GridLines v n
  , saTicks     :: Ticks v n
  }

type instance V (SingleAxis b v n) = v
type instance N (SingleAxis b v n) = n

instance (TypeableFloat n, Enum n, Renderable (Text n) b)
    => Default (SingleAxis b V2 n) where
  def = SingleAxis
    { saLabel      = def
    , saLine       = def
    , saTickLabel  = def
    , saGridLines  = def
    , saTicks      = def
    , saScaling    = def
    }

instance Functor f => HasTicks f (SingleAxis b v n) where
  bothTicks = lens saTicks (\sa ticks -> sa {saTicks = ticks})

instance Functor f => HasMajorTicks f (SingleAxis b v n) where
  majorTicks = bothTicks . majorTicks

instance Functor f => HasMinorTicks f (SingleAxis b v n) where
  minorTicks = bothTicks . minorTicks

instance Functor f => HasAxisLabel f (SingleAxis b v n) b where
  axisLabel = lens saLabel (\sa l -> sa {saLabel = l})

instance Functor f => HasTickLabels f (SingleAxis b v n) b where
  tickLabel = lens saTickLabel (\sa tl -> sa {saTickLabel = tl})

instance Functor f => HasAxisLine f (SingleAxis b v n) where
  axisLine = lens saLine (\sa l -> sa {saLine = l})

instance Functor f => HasGridLines f (SingleAxis b v n) where
  gridLines = lens saGridLines (\sa l -> sa {saGridLines = l})

instance Functor f => HasMajorGridLines f (SingleAxis b v n) where
  majorGridLines = gridLines . majorGridLines

instance Functor f => HasMinorGridLines f (SingleAxis b v n) where
  minorGridLines = gridLines . minorGridLines

instance Functor f => HasAxisScaling f (SingleAxis b v n) where
  axisScaling = lens saScaling (\sa s -> sa {saScaling = s})

-- singleAxisScale :: Lens' (SingleAxis b v n) AxisScale
-- singleAxisScale = lens saScale (\sa s -> sa {saScale = s})

-- singleAxisBound :: Lens' (SingleAxis b v n) (Bound n)
-- singleAxisBound = lens saBounds (\sa b -> sa {saBounds = b})

------------------------------------------------------------------------
-- Axis type
------------------------------------------------------------------------

-- Base space ----------------------------------------------------------

-- | This family is used so that we can say (Axis Polar) but use V2 for the
--   underlying diagram.
type family BaseSpace (c :: * -> *) :: * -> *

type instance BaseSpace V2      = V2
type instance BaseSpace Complex = V2
type instance BaseSpace Polar   = V2
type instance BaseSpace V3      = V3

-- Axis data type ------------------------------------------------------

-- | Axis is the data type that holds all the nessessary information to render
--   a plot. Common 'LensLike's used for the axis (see haddock's
--   instances for a more comprehensive list):
--
-- * 'axisStyle'    - customise the 'AxisStyle'
-- * 'legend'       - customise the 'Legend'
-- * 'colourBar'    - customise the 'ColourBar'
-- * 'currentPlots' - current plots in the 'Axis'
-- * 'finalPlots'   - changes to the plots just before rendering
-- * 'axes'         - changes to each 'SingleAxis'
--
--          * 'xAxis' - the x-axis
--          * 'yAxis' - the y-axis
--          * 'zAxis' - the z-axis
--
--   The following 'LensLike's can be used on the on all the axes by
--   applying it the to 'Axis' or can be used on a 'SingleAxis' by using
--   it in combination with a specific axis (like 'xAxis').
--
--   * 'axisLabel'   - customise the 'MinorTicks'
--   * 'tickLabel'   - customise the 'TickLabels'
--   * 'minorTicks'  - customise the 'MinorTicks'
--   * 'majorTicks'  - customise the 'MajorTicks'
--   * 'gridLines'   - customise the 'GridLines'
--   * 'axisLine'    - customise the 'AxisLine'
--   * 'axisScaling' - customise the 'AxisScaling'
--
data Axis b c n = Axis
  { _axisStyle   :: AxisStyle b (BaseSpace c) n
  , _colourBar   :: ColourBar b n
  , _legend      :: Legend b n
  -- , _axisTitle      :: AxisTitle

  , _axisPlots   :: [DynamicPlot b (BaseSpace c) n]
  , _plotModifier :: Endo (StyledPlot b (BaseSpace c) n)

  -- the v in each axis is only used for the style
  , _axes        :: c (SingleAxis b (BaseSpace c) n)
  } deriving Typeable

-- | Lens onto the separate axes of an axis. Allows changing the
--   coordinate system as long as the 'BaseSpace' is the same.
--
-- @
-- 'axes' :: 'Lens'' ('Axis' b c n) (c ('SingleAxis' b v n))
-- @
axes :: (v ~ BaseSpace c, v ~ BaseSpace c')
     => Lens (Axis b c  n)
             (Axis b c' n)
             (c  (SingleAxis b v n))
             (c' (SingleAxis b v n))
axes = lens _axes (\(Axis a1 a2 a3 a4 a5 _) a6 -> Axis a1 a2 a3 a4 a5 a6)

-- | The list of plots currently in the axis.
axisPlots :: BaseSpace c ~ v => Lens' (Axis b c n) [DynamicPlot b v n]
axisPlots = lens _axisPlots (\a ps -> a {_axisPlots = ps})

-- | Traversal over the current plots in the axis.
--
--   For example, to make all 'ScatterPlot's currently in the axis use a
--   'connectingLine', you can write
--
-- @
-- 'finalPlots' . 'connectingLine' .= 'True'
-- @
currentPlots :: BaseSpace c ~ v => Traversal' (Axis b c n) (DynamicPlot b v n)
currentPlots = axisPlots . traversed

-- | Setter over the final plot before the axis is rendered.
--
--   For example, to make all 'ScatterPlot's in the axis use a
--   'connectingLine' (both currently in the axis and ones added later),
--   you can add
--
-- @
-- 'finalPlots' . 'connectingLine' .= 'True'
-- @
--
finalPlots :: BaseSpace c ~ v => Setter' (Axis b c n) (StyledPlot b v n)
finalPlots = sets $ \f a -> a {_plotModifier = _plotModifier a <> Endo f}

-- | Lens onto the modifier set by 'finalPlots'. This gets applied to
--   all plots in the axis, just before they are rendered.
plotModifier :: BaseSpace c ~ v => Lens' (Axis b c n) (Endo (StyledPlot b v n))
plotModifier = lens _plotModifier (\a f -> a {_plotModifier = f})

-- Axis instances ------------------------------------------------------

type instance V (Axis b v n) = BaseSpace v
type instance N (Axis b v n) = n

instance (Applicative f, Traversable c) => HasTicks f (Axis b c n) where
  bothTicks = axes . traverse . bothTicks

instance (Applicative f, Traversable c) => HasMajorTicks f (Axis b c n) where
  majorTicks = axes . traverse . majorTicks

instance (Applicative f, Traversable c) => HasMinorTicks f (Axis b c n) where
  minorTicks = axes . traverse . minorTicks

instance (Applicative f, Traversable c) => HasGridLines f (Axis b c n) where
  gridLines = axes . traverse . gridLines

instance (Applicative f, Traversable c) => HasMajorGridLines f (Axis b c n) where
  majorGridLines = axes . traverse . majorGridLines

instance (Applicative f, Traversable c) => HasMinorGridLines f (Axis b c n) where
  minorGridLines = axes . traverse . minorGridLines

instance (Applicative f, Traversable c) => HasAxisLabel f (Axis b c n) b where
  axisLabel = axes . traverse . axisLabel

instance (Applicative f, Traversable c) => HasTickLabels f (Axis b c n) b where
  tickLabel = axes . traverse . tickLabel

instance (Applicative f, Traversable c) => HasAxisScaling f (Axis b c n) where
  axisScaling = axes . traverse . axisScaling

instance Settable f => HasPlotOptions f (Axis b c n) b where
  plotOptions = finalPlots . plotOptions

instance Settable f => HasPlotStyle f (Axis b c n) b where
  plotStyle = finalPlots . plotStyle

instance HasLegend (Axis b c n) b where
  legend = lens _legend (\a l -> a {_legend = l})

-- | The size used for the rendered axis.
axisSize :: (HasLinearMap c, Num n, Ord n) => Lens' (Axis b c n) (SizeSpec c n)
axisSize = axes . column renderSize . iso mkSizeSpec getSpec -- column axisScaling . asSizeSpec -- iso mkSizeSpec getSpec

instance HasAxisStyle (Axis b v n) b where
  axisStyle = lens _axisStyle (\a sty -> a {_axisStyle = sty})

instance HasColourBar (Axis b v n) b where
  colourBar = lens _colourBar (\a cb -> a {_colourBar = cb})

-- Axis functions ------------------------------------------------------

-- $plotable
-- The 'Plotable' class defines ways of converting the data type to a
-- diagram for some axis. There are several variants for adding an axis
-- with constraints @('InSpace' v n a, 'Plotable' a b)@:
--
-- @
-- 'addPlotable'  ::           a -> 'PlotState' a b -> 'AxisState' b v n
-- 'addPlotable''   ::           a ->                  'AxisState' b v n
-- @
--
-- The last argument is a 'PlotState' so you can use @do@ notation to
-- make adjustments to the plot. The @L@ suffix stands for \"legend\",
-- it is equivalent of using 'addLegendEntry' in the 'PlotState'. Since
-- legend entries are so common it has it's own suffix. The following
-- are equivalent:
--
-- @
-- myaxis = 'r2Axis' &~ do
--   'addPlotable'' myplot $ do
--     'addLegendEntry' "my plot"
-- @
--
-- Most of the time you won't use these functions directly. However,
-- other plotting functions follow this naming convention where instead
-- of @a@, it takes the data needed to make the plot.

-- | Add a 'Plotable' 'Plot' to an 'Axis'.
addPlot
  :: (InSpace (BaseSpace c) n p, MonadState (Axis b c n) m, Plotable p b)
  => Plot p b -- ^ the plot
  -> m ()     -- ^ add plot to the 'Axis'
addPlot p = axisPlots <>= [DynamicPlot p]

-- | Add something 'Plotable' to the 'Axis' with a statefull modification
--   of the 'Plot'.
addPlotable
  :: (InSpace (BaseSpace c) n p, MonadState (Axis b c n) m, Plotable p b)
  => p -- ^ the raw plot
  -> State (Plot p b) () -- ^ changes to the plot
  -> m () -- ^ add plot to the 'Axis'
addPlotable p s = addPlot $ execState s (mkPlot p)

-- | Simple version of 'AddPlotable' without any changes 'Plot'.
addPlotable'
  :: (InSpace (BaseSpace v) n p, MonadState (Axis b v n) m, Plotable p b)
  => p    -- ^ the raw plot
  -> m () -- ^ add plot to the 'Axis'
addPlotable' p = addPlotable p (return ())

------------------------------------------------------------------------
-- Predefined axes
------------------------------------------------------------------------

-- | The default axis for plots in the 'V2' coordinate system.
r2Axis
  :: (TypeableFloat n,
     Enum n,
     Renderable (Text n) b,
     Renderable (Path V2 n) b)
  => Axis b V2 n
r2Axis = Axis
  { _axisStyle = fadedColours
  , _colourBar = defColourBar

  , _legend       = def
  , _axisPlots    = []
  , _plotModifier = mempty

  , _axes = pure def
  }

-- The x-axis ----------------------------------------------------------

-- | Lens onto the x-axis of an 'Axis'.
xAxis :: R1 c => Lens' (Axis b c n) (SingleAxis b (BaseSpace c) n)
xAxis = axes . _x

-- | The label for the x-axis. Shorthand for @'xAxis' . 'axisLabelText'@.
xLabel :: R1 c => Lens' (Axis b c n) String
xLabel = xAxis . axisLabelText

-- | The minimum x value for the axis. If the value if 'Nothing' (the
--   'Default'), the bounds will be infered by the plots in the axis.
xMin :: R1 c => Lens' (Axis b c n) (Maybe n)
xMin = xAxis . boundMin

-- | The minimum x value for the axis. If the value if 'Nothing' (the
--   'Default'), the bounds will be infered by the plots in the axis.
xMax :: R1 c => Lens' (Axis b c n) (Maybe n)
xMax = xAxis . boundMax

-- The y-axis ----------------------------------------------------------

-- | Lens onto the y-axis of an 'Axis'.
yAxis :: R2 c => Lens' (Axis b c n) (SingleAxis b (BaseSpace c) n)
yAxis = axes . _y

-- | The label for the y-axis. Shorthand for @'yAxis' . 'axisLabelText'@.
yLabel :: R2 c => Lens' (Axis b c n) String
yLabel = yAxis . axisLabelText

-- | The minimum y value for the axis. If the value if 'Nothing' (the
--   'Default'), the bounds will be infered by the plots in the axis.
yMin :: R2 c => Lens' (Axis b c n) (Maybe n)
yMin = yAxis . boundMin

-- | The minimum y value for the axis. If the value if 'Nothing' (the
--   'Default'), the bounds will be infered by the plots in the axis.
yMax :: R2 c => Lens' (Axis b c n) (Maybe n)
yMax = yAxis . boundMax

-- The z-axis ----------------------------------------------------------

-- | Lens onto the z-axis of an 'Axis'.
zAxis :: R3 c => Lens' (Axis b c n) (SingleAxis b (BaseSpace c) n)
zAxis = axes . _z

-- | The label for the z-axis. Shorthand for @'zAxis' . 'axisLabelText'@.
zLabel :: R3 c => Lens' (Axis b c n) String
zLabel = zAxis . axisLabelText

-- | The minimum z value for the axis. If the value if 'Nothing' (the
--   'Default'), the bounds will be infered by the plots in the axis.
zMin :: R3 c => Lens' (Axis b c n) (Maybe n)
zMin = zAxis . boundMin

-- | The minimum z value for the axis. If the value if 'Nothing' (the
--   'Default'), the bounds will be infered by the plots in the axis.
zMax :: R3 c => Lens' (Axis b c n) (Maybe n)
zMax = zAxis . boundMax

-- R3 Axis

-- instance (TypeableFloat n, Enum n, Renderable (Text n) b, Renderable (Path V2 n) b)
--     => Default (Axis b V3 n) where
--   def = Axis
--           { _axisTitle      = Nothing
--           , _axisSize       = mkWidth 300
--           , _axisPlots      = []
--           , _axisLegend     = def
--           , _axisTheme      = coolTheme
--           , _axisLinearMap  = isometricProjection
--           , _axisAxisBounds = Bounds $ pure def
--           , _axisGridLines  = pure def
--           , _axisLabels     = pure def
--           , _axisScaling    = pure def
--           , _axisTickLabels = pure def
--           , _axisTicks      = pure def
--           , _axisLines      = pure def
--           }

-- R3 Axis

-- polarAxis :: (TypeableFloat n, Enum n, Renderable (Text n) b, Renderable (Path V2 n) b) => Axis b Polar n
-- polarAxis = Axis
--   { _axisTitle      = Nothing
--   , _axisSize       = mkWidth 300
--   , _axisPlots      = []
--   , _axisLegend     = def
--   , _axisColourBar  = defColourBar
--   , _axisAxisStyle  = fadedColours
--   , _axisAxisBounds = Bounds $ pure def
--   , _axisGridLines  = pure def
--   , _axisLabels     = Polar $ V2 def (def & axisLabelFunction %~ (fmap . fmap $ rotateBy (1/4))
--                                           & axisLabelGap .~ 40
--                                      )
--   , _axisScaling    = pure def
--   , _axisTickLabels = pure def
--   , _axisTicks      = pure def
--   , _axisLines      = pure def
--   , _axisScale      = pure def
--   -- , _axisPP         = def
--   }

