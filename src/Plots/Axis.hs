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
-- necessary infomation to be rendered into a 'Diagram'.
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
  , colourBarRange

    -- * Predefined axes
  , r2Axis
  , r3Axis
  , polarAxis

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

    -- ** r-axis
  , rAxis
  , rLabel
  , rMax

    -- ** theta-axis
  , thetaAxis
  , thetaLabel

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
import           Plots.Axis.Title
import           Plots.Axis.Ticks
import           Plots.Legend
import           Plots.Style
import           Plots.Types

import           Linear

------------------------------------------------------------------------
-- Axis data type
------------------------------------------------------------------------

-- Single axis ---------------------------------------------------------

-- | Render information for a single axis line.
data SingleAxis v = SingleAxis
-- note the the v is only present for Style v
  { saLabel     :: AxisLabel v
  , saLine      :: AxisLine v
  , saTickLabel :: TickLabels v
  , saScaling   :: AxisScaling
  , saGridLines :: GridLines v
  , saTicks     :: Ticks v
  , saVisible   :: Bool
  }

type instance V (SingleAxis v) = v
type instance N (SingleAxis v) = Double

instance Default (SingleAxis V2) where
  def = SingleAxis
    { saLabel      = def
    , saLine       = def
    , saTickLabel  = def
    , saGridLines  = def
    , saTicks      = def
    , saScaling    = def
    , saVisible    = True
    }

instance Default (SingleAxis V3) where
  def = SingleAxis
    { saLabel      = def
    , saLine       = def
    , saTickLabel  = def
    , saGridLines  = def
    , saTicks      = def
    , saScaling    = def
    , saVisible    = True
    }

instance Functor f => HasTicks f (SingleAxis v) where
  bothTicks = lens saTicks (\sa ticks -> sa {saTicks = ticks})

instance Functor f => HasMajorTicks f (SingleAxis v) where
  majorTicks = bothTicks . majorTicks

instance Functor f => HasMinorTicks f (SingleAxis v) where
  minorTicks = bothTicks . minorTicks

instance Functor f => HasAxisLabel f (SingleAxis v) where
  axisLabel = lens saLabel (\sa l -> sa {saLabel = l})

instance Functor f => HasTickLabels f (SingleAxis v) where
  tickLabel = lens saTickLabel (\sa tl -> sa {saTickLabel = tl})

instance Functor f => HasAxisLine f (SingleAxis v) where
  axisLine = lens saLine (\sa l -> sa {saLine = l})

instance Functor f => HasGridLines f (SingleAxis v) where
  gridLines = lens saGridLines (\sa l -> sa {saGridLines = l})

instance Functor f => HasMajorGridLines f (SingleAxis v) where
  majorGridLines = gridLines . majorGridLines

instance Functor f => HasMinorGridLines f (SingleAxis v) where
  minorGridLines = gridLines . minorGridLines

instance Functor f => HasAxisScaling f (SingleAxis v) where
  axisScaling = lens saScaling (\sa s -> sa {saScaling = s})

instance HasVisibility (SingleAxis v) where
  visible = lens saVisible (\sa b -> sa {saVisible = b})

-- singleAxisScale :: Lens' (SingleAxis v) AxisScale
-- singleAxisScale = lens saScale (\sa s -> sa {saScale = s})

-- singleAxisBound :: Lens' (SingleAxis v) (Bound n)
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

-- | Axis is the data type that holds all the necessary information to render
--   a plot. Common 'LensLike's used for the axis (see haddock's
--   instances for a more comprehensive list):
--
--   * 'axisStyle'    - customise the 'AxisStyle'
--   * 'legend'       - customise the 'Legend'
--   * 'colourBar'    - customise the 'ColourBar'
--   * 'currentPlots' - current plots in the 'Axis'
--   * 'finalPlots'   - changes to the plots just before rendering
--   * 'axes'         - changes to each 'SingleAxis'
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
--   Plots are usually added to the axis using specific functions for
--   those plots ('Plots.Types.Line.linePlot, 'Plots.Types.Bar.barPlot').
--   These functions use 'addPlotable' to add the plot to the axis.
data Axis c = Axis
  { _axisStyle   :: AxisStyle (BaseSpace c)
  , _colourBar   :: ColourBar
  , _colourBarR  :: (Double,Double)
  , _legend      :: Legend
  , _axisTitle   :: Title (BaseSpace c)
  -- , _axisTitle      :: AxisTitle

  , _axisPlots   :: [DynamicPlot (BaseSpace c)]
  , _plotModifier :: Endo (StyledPlot (BaseSpace c))

  -- the v in each axis is only used for the style
  , _axes        :: c (SingleAxis (BaseSpace c))
  } deriving Typeable

-- | Lens onto the separate axes of an axis. Allows changing the
--   coordinate system as long as the 'BaseSpace' is the same.
--
-- @
-- 'axes' :: 'Lens'' ('Axis' c) (c ('SingleAxis' v))
-- @
axes :: (v ~ BaseSpace c, v ~ BaseSpace c')
     => Lens (Axis c)
             (Axis c')
             (c  (SingleAxis v))
             (c' (SingleAxis v))
axes = lens _axes (\(Axis a1 a2 a3 a4 a5 a6 a7 _) a8 -> Axis a1 a2 a3 a4 a5 a6 a7 a8)

-- | The list of plots currently in the axis.
axisPlots :: BaseSpace c ~ v => Lens' (Axis c) [DynamicPlot v]
axisPlots = lens _axisPlots (\a ps -> a {_axisPlots = ps})

-- | Traversal over the current plots in the axis.
--
--   For example, to make all 'ScatterPlot's currently in the axis use a
--   'connectingLine', you can write
--
-- @
-- 'finalPlots' . 'connectingLine' .= 'True'
-- @
currentPlots :: BaseSpace c ~ v => Traversal' (Axis c) (DynamicPlot v)
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
finalPlots :: BaseSpace c ~ v => Setter' (Axis c) (StyledPlot v)
finalPlots = sets $ \f a -> a {_plotModifier = _plotModifier a <> Endo f}

-- | Lens onto the modifier set by 'finalPlots'. This gets applied to
--   all plots in the axis, just before they are rendered.
plotModifier :: BaseSpace c ~ v => Lens' (Axis c) (Endo (StyledPlot v))
plotModifier = lens _plotModifier (\a f -> a {_plotModifier = f})

-- Axis instances ------------------------------------------------------

type instance V (Axis c) = BaseSpace c
type instance N (Axis c) = Double

instance (Applicative f, Traversable c) => HasTicks f (Axis c) where
  bothTicks = axes . traverse . bothTicks

instance (Applicative f, Traversable c) => HasMajorTicks f (Axis c) where
  majorTicks = axes . traverse . majorTicks

instance (Applicative f, Traversable c) => HasMinorTicks f (Axis c) where
  minorTicks = axes . traverse . minorTicks

instance (Applicative f, Traversable c) => HasGridLines f (Axis c) where
  gridLines = axes . traverse . gridLines

instance (Applicative f, Traversable c) => HasMajorGridLines f (Axis c) where
  majorGridLines = axes . traverse . majorGridLines

instance (Applicative f, Traversable c) => HasMinorGridLines f (Axis c) where
  minorGridLines = axes . traverse . minorGridLines

instance (Applicative f, Traversable c) => HasAxisLine f (Axis c) where
  axisLine = axes . traverse . axisLine

instance (Applicative f, Traversable c) => HasAxisLabel f (Axis c) where
  axisLabel = axes . traverse . axisLabel

instance (Applicative f, Traversable c) => HasTickLabels f (Axis c) where
  tickLabel = axes . traverse . tickLabel

instance (Applicative f, Traversable c) => HasAxisScaling f (Axis c) where
  axisScaling = axes . traverse . axisScaling

instance Settable f => HasPlotOptions f (Axis c) where
  plotOptions = finalPlots . plotOptions

instance Settable f => HasPlotStyle f (Axis c) where
  plotStyle = finalPlots . plotStyle

instance HasLegend (Axis c) where
  legend = lens _legend (\a l -> a {_legend = l})

instance HasTitle (Axis c) where
  title = lens _axisTitle (\a t -> a {_axisTitle = t})

-- | The size used for the rendered axis.
axisSize :: HasLinearMap c => Lens' (Axis c) (SizeSpec c Double)
axisSize = axes . column renderSize . iso mkSizeSpec getSpec

-- | The range used for the colour bar limits. This is automatically set
--   when using 'heatMap' or 'heatMap''
colourBarRange :: Lens' (Axis v) (Double,Double)
colourBarRange = lens _colourBarR (\a r -> a {_colourBarR = r})

instance HasAxisStyle (Axis v) where
  axisStyle = lens _axisStyle (\a sty -> a {_axisStyle = sty})

instance HasColourBar (Axis v) where
  colourBar = lens _colourBar (\a cb -> a {_colourBar = cb})

-- Axis functions ------------------------------------------------------

-- $plotable
-- The 'Plotable' class defines ways of converting the data type to a
-- diagram for some axis. There are several variants for adding an axis
-- with constraints @('InSpace' v a, 'Plotable' a b)@:
--
-- @
-- 'addPlotable'  :: a -> 'PlotState' a b -> 'AxisState' v
-- 'addPlotable'' :: a ->                  'AxisState' v
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
  :: (InSpace (BaseSpace c) Double p, MonadState (Axis c) m, Plotable p)
  => Plot p -- ^ the plot
  -> m ()   -- ^ add plot to the 'Axis'
addPlot p = axisPlots <>= [DynamicPlot p]

-- | Add something 'Plotable' to the 'Axis' with a stateful modification
--   of the 'Plot'.
addPlotable
  :: (InSpace (BaseSpace c) Double p, MonadState (Axis c) m, Plotable p, HasLinearMap (BaseSpace c))
  => p -- ^ the raw plot
  -> State (Plot p) () -- ^ changes to the plot
  -> m () -- ^ add plot to the 'Axis'
addPlotable p s = addPlot $ execState s (mkPlot p)

-- | Simple version of 'AddPlotable' without any changes 'Plot'.
addPlotable'
  :: (InSpace (BaseSpace c) Double p, MonadState (Axis c) m, Plotable p, HasLinearMap (BaseSpace c))
  => p    -- ^ the raw plot
  -> m () -- ^ add plot to the 'Axis'
addPlotable' p = addPlotable p (return ())

------------------------------------------------------------------------
-- Predefined axes
------------------------------------------------------------------------

-- | The default axis for plots in the 'V2' coordinate system.
r2Axis :: Axis V2
r2Axis = Axis
  { _axisStyle  = fadedColours
  , _colourBar  = defColourBar
  , _colourBarR = (0,1)
  , _axisTitle  = def

  , _legend       = def
  , _axisPlots    = []
  , _plotModifier = mempty

  , _axes = pure def
  }

-- | The default axis for plots in the 'V2' coordinate system.
r3Axis :: Axis V3
r3Axis = Axis
  { _axisStyle  = fadedColours3D
  , _colourBar  = defColourBar
  , _colourBarR = (0,1)
  , _axisTitle  = def

  , _legend       = def
  , _axisPlots    = []
  , _plotModifier = mempty

  , _axes = pure def
  }

-- The x-axis ----------------------------------------------------------

-- | Lens onto the x-axis of an 'Axis'.
xAxis :: R1 c => Lens' (Axis c) (SingleAxis (BaseSpace c))
xAxis = axes . _x

-- | The label for the x-axis. Shorthand for @'xAxis' . 'axisLabelText'@.
xLabel :: R1 c => Lens' (Axis c) String
xLabel = xAxis . axisLabelText

-- | The minimum x value for the axis. If the value if 'Nothing' (the
--   'Default'), the bounds will be infered by the plots in the axis.
xMin :: R1 c => Lens' (Axis c) (Maybe Double)
xMin = xAxis . boundMin

-- | The minimum x value for the axis. If the value if 'Nothing' (the
--   'Default'), the bounds will be infered by the plots in the axis.
xMax :: R1 c => Lens' (Axis c) (Maybe Double)
xMax = xAxis . boundMax

-- The y-axis ----------------------------------------------------------

-- | Lens onto the y-axis of an 'Axis'.
yAxis :: R2 c => Lens' (Axis c) (SingleAxis (BaseSpace c))
yAxis = axes . _y

-- | The label for the y-axis. Shorthand for @'yAxis' . 'axisLabelText'@.
yLabel :: R2 c => Lens' (Axis c) String
yLabel = yAxis . axisLabelText

-- | The minimum y value for the axis. If the value if 'Nothing' (the
--   'Default'), the bounds will be infered by the plots in the axis.
yMin :: R2 c => Lens' (Axis c) (Maybe Double)
yMin = yAxis . boundMin

-- | The minimum y value for the axis. If the value if 'Nothing' (the
--   'Default'), the bounds will be infered by the plots in the axis.
yMax :: R2 c => Lens' (Axis c) (Maybe Double)
yMax = yAxis . boundMax

-- The z-axis ----------------------------------------------------------

-- | Lens onto the z-axis of an 'Axis'.
zAxis :: R3 c => Lens' (Axis c) (SingleAxis (BaseSpace c))
zAxis = axes . _z

-- | The label for the z-axis. Shorthand for @'zAxis' . 'axisLabelText'@.
zLabel :: R3 c => Lens' (Axis c) String
zLabel = zAxis . axisLabelText

-- | The minimum z value for the axis. If the value if 'Nothing' (the
--   'Default'), the bounds will be infered by the plots in the axis.
zMin :: R3 c => Lens' (Axis c) (Maybe Double)
zMin = zAxis . boundMin

-- | The minimum z value for the axis. If the value if 'Nothing' (the
--   'Default'), the bounds will be infered by the plots in the axis.
zMax :: R3 c => Lens' (Axis c) (Maybe Double)
zMax = zAxis . boundMax

-- The r-axis ----------------------------------------------------------

-- | Lens onto the radial axis of an 'Axis'.
rAxis :: Radial c => Lens' (Axis c) (SingleAxis (BaseSpace c))
rAxis = axes . _radial

-- | The label for the radial axis. Shorthand for @'rAxis' . 'axisLabelText'@.
rLabel :: Radial c => Lens' (Axis c) String
rLabel = rAxis . axisLabelText

-- | The minimum z value for the axis. If the value if 'Nothing' (the
--   'Default'), the bounds will be infered by the plots in the axis.
-- rMin :: R3 c => Lens' (Axis c) (Maybe Double)
-- rMin = zAxis . boundMin

-- | The minimum radial value for the axis. If the value if 'Nothing'
--   (the 'Default'), the bounds will be infered by the plots in the
--   axis.
rMax :: Radial c => Lens' (Axis c) (Maybe Double)
rMax = rAxis . boundMax

-- The theta-axis ------------------------------------------------------

-- | Lens onto the radial axis of an 'Axis'.
thetaAxis :: Circle c => Lens' (Axis c) (SingleAxis (BaseSpace c))
thetaAxis = axes . el etheta

-- | The label for the radial axis. Shorthand for @'rAxis' . 'axisLabelText'@.
thetaLabel :: Circle c => Lens' (Axis c) String
thetaLabel = thetaAxis . axisLabelText


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

polarAxis :: Axis Polar
polarAxis = Axis
  { _axisStyle  = fadedColours
  , _colourBar  = defColourBar
  , _colourBarR = (0,1)
  , _axisTitle  = def

  , _legend       = def
  , _axisPlots    = []
  , _plotModifier = mempty

  , _axes = pure def
  }

