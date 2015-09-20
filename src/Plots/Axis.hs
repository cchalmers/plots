{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Plots.Axis
-- Copyright   :  (C) 2015 Christopher Chalmers
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Christopher Chalmers
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Low level module defining the axis type along with other
-- miscellaneous axis features.
--
----------------------------------------------------------------------------
module Plots.Axis
  ( -- * Axis type
    Axis
  , axes
  , axisPlots
  , axisLegend
  , axisSize
  , axisScale

    -- ** Base space
  , BaseSpace

    -- * Axis plots
  , addPlotable
  , addPlotable'

    -- * Single axis
  , SingleAxis
  , singleAxisSize
  , singleAxisScale
  , singleAxisBound

    -- * Axis lines
  , AxisLine
  , HasAxisLine (..)
  , AxisLineType (..)

    -- * Scaling
  , AxisScaling
  , HasAxisScaling (..)
  , ScaleMode (..)
  , UniformScaleStrategy (..)

    -- * Specific axes
    -- ** x-axis
  , xAxis
  , xAxisLabel

    -- ** y-axis
  , yAxis
  , yAxisLabel
  ) where

import           Data.Complex
import           Data.Default
import           Data.Functor.Rep
import           Data.Monoid.Recommend
import           Data.Typeable

import           Diagrams.Coordinates.Polar
import           Diagrams.Prelude
import           Diagrams.TwoD.Text

import           Control.Monad.State
import           Plots.Axis.ColourBar
import           Plots.Axis.Grid
import           Plots.Axis.Labels
import           Plots.Axis.Line
import           Plots.Axis.Ticks
import           Plots.Legend
import           Plots.Style
import           Plots.Types

import           Linear

------------------------------------------------------------------------
-- Axis scale
------------------------------------------------------------------------

data ScaleMode
  = AutoScale
  | NoScale
  | Stretch
  | UniformScale UniformScaleStrategy
  deriving (Show, Read)

data UniformScaleStrategy
  = AutoUniformScale
  | UnitOnly
  | ChangeVerticalLimits
  | ChangeHorizontalLimits
  deriving (Show, Read)

data AxisScaling n = Scaling
  { asRatio     :: Recommend n
  , asPostScale :: Maybe n
  , asMode      :: ScaleMode
  , asEnlarge   :: Maybe (Recommend n)
  }

type instance N (AxisScaling n) = n

-- XXX document / sort out this

class HasAxisScaling a where
  axisScaling :: Lens' a (AxisScaling (N a))

  scaleAspectRatio :: Lens' a (Recommend (N a))
  scaleAspectRatio = axisScaling . lens asRatio (\as r -> as {asRatio = r})

  scalePostScale :: Lens' a (Maybe (N a))
  scalePostScale = axisScaling
                 . lens asPostScale (\as r -> as {asPostScale = r})

  scaleMode :: Lens' a ScaleMode
  scaleMode = axisScaling . lens asMode (\as r -> as {asMode = r})

  axisScaleEnlarge :: Lens' a (Maybe (Recommend (N a)))
  axisScaleEnlarge = axisScaling . lens asEnlarge (\as r -> as {asEnlarge = r})

instance HasAxisScaling (AxisScaling n) where
  axisScaling = id

instance Fractional n => Default (AxisScaling n) where
  def = Scaling
    { asRatio     = Recommend 1
    , asPostScale = Nothing
    , asMode      = AutoScale
    , asEnlarge   = Just $ Recommend 0.1
    }

------------------------------------------------------------------------
-- Axis data type
------------------------------------------------------------------------

-- Single axis ---------------------------------------------------------

data SingleAxis b v n = SingleAxis
-- note the the v is only present for Style v n
  {
  -- labels
    saLabel     :: AxisLabel b v n
  , saLine      :: AxisLine v n
  , saTickLabel :: TickLabels b v n

  -- markers
  , saGridLines :: GridLines v n
  , saTicks     :: Ticks v n

  -- size / scale
  , saScaling   :: AxisScaling n
  , saSize      :: Maybe n -- used for SizeSpec
  , saBounds    :: Bound n
  , saScale     :: AxisScale
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
    , saSize       = Just 300
    , saBounds     = def
    , saScale      = def
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

instance HasAxisScaling (SingleAxis b v n) where
  axisScaling = lens saScaling (\sa tl -> sa {saScaling = tl})

instance Functor f => HasAxisLine f (SingleAxis b v n) where
  axisLine = lens saLine (\sa l -> sa {saLine = l})

instance Functor f => HasGridLines f (SingleAxis b v n) where
  gridLines = lens saGridLines (\sa l -> sa {saGridLines = l})

singleAxisScale :: Lens' (SingleAxis b v n) AxisScale
singleAxisScale = lens saScale (\sa s -> sa {saScale = s})

singleAxisBound :: Lens' (SingleAxis b v n) (Bound n)
singleAxisBound = lens saBounds (\sa b -> sa {saBounds = b})

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
--   a plot. The idea is to use one of the default axis, customise, add plots
--   and render using @drawAxis@.
data Axis b c n = Axis
  { -- These lenses are not being exported, they're just here for instances.
    -- _axisAxisBounds :: Bounds v n
    _axisAxisStyle :: AxisStyle b (BaseSpace c) n
  , _axisColourBar :: ColourBar b n
  -- , _axisPP         :: PlotProperties b (BaseSpace v) n


  , _axisLegend    :: Legend b n
  , _axisPlots     :: [DynamicPlot b (BaseSpace c) n]
  -- , _axisTitle      :: Maybe String

  -- the v in each axis is only used for @'Style' v n@
  , _axes          :: c (SingleAxis b (BaseSpace c) n)
  } deriving Typeable

makeLenses ''Axis

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

instance (Applicative f, Traversable c) => HasAxisLabel f (Axis b c n) b where
  axisLabel = axes . traverse . axisLabel

instance (Applicative f, Traversable c) => HasTickLabels f (Axis b c n) b where
  tickLabel = axes . traverse . tickLabel

instance Settable f => HasPlotStyle f (Axis b c n) b where
  plotStyle = axisPlots . traverse . plotStyle

instance HasLegend (Axis b c n) b where
  legend = axisLegend

axisSize :: (Representable c, Num n, Ord n) => Lens' (Axis b c n) (SizeSpec c n)
axisSize = axes . column singleAxisSize . iso mkSizeSpec getSpec

singleAxisSize :: Lens' (SingleAxis b v n) (Maybe n)
singleAxisSize = lens saSize (\sa s -> sa {saSize = s})

axisScale :: Representable c => Lens' (Axis b c n) (c AxisScale)
axisScale = axes . column singleAxisScale

-- axisLine :: E v -> Lens' (Axis b v n) (AxisLine n)
-- axisLine (E l) = axisLines . l

-- instance HasBounds (Axis b v n) v where
--   bounds = axisAxisBounds

instance HasAxisStyle (Axis b v n) b where
  axisStyle = axisAxisStyle

instance HasColourBar (Axis b v n) b where
  colourBar = axisColourBar

-- Axis functions ------------------------------------------------------

-- | Add something 'Plotable' to the axis with a statefull modification
--   of the 'Plot'.
addPlotable
  :: (InSpace (BaseSpace v) n p, MonadState (Axis b v n) m, Plotable p b)
  => p -> State (Plot p b) () -> m ()
addPlotable p s = axisPlots <>= [DynamicPlot $ execState s (mkPlot p)]

-- | Simple version of 'AddPlotable' without any changes 'Plot'.
addPlotable'
  :: (InSpace (BaseSpace v) n p, MonadState (Axis b v n) m, Plotable p b)
  => p -> m ()
addPlotable' p = addPlotable p (return ())

------------------------------------------------------------------------
-- R2 Axis
------------------------------------------------------------------------

instance (TypeableFloat n,
          Enum n,
          Renderable (Text n) b,
          Renderable (Path V2 n) b)
    => Default (Axis b V2 n) where
  def = Axis
    { _axisAxisStyle  = fadedColours
    , _axisColourBar  = defColourBar

    , _axisLegend     = def
    , _axisPlots      = []

    , _axes = pure def
    }

-- The x-axis ----------------------------------------------------------

-- | Lens onto the x-axis of an 'Axis'.
xAxis :: R1 c => Lens' (Axis b c n) (SingleAxis b (BaseSpace c) n)
xAxis = axes . _x

xAxisLabel :: R1 c => Lens' (Axis b c n) String
xAxisLabel = xAxis . axisLabelText

-- The y-axis ----------------------------------------------------------

yAxis :: R2 c => Lens' (Axis b c n) (SingleAxis b (BaseSpace c) n)
yAxis = axes . _y

yAxisLabel :: R2 c => Lens' (Axis b c n) String
yAxisLabel = yAxis . axisLabelText

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

