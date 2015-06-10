{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Plots.Axis where

import           Control.Lens          hiding (lmap, transform, ( # ))
import           Data.Default
import           Data.Monoid.Recommend
import           Data.Typeable

import           Diagrams.Prelude      as D hiding (under, view)
import           Diagrams.TwoD.Text

import           Plots.Axis.ColourBar
import           Plots.Axis.Grid
import           Plots.Axis.Labels
import           Plots.Axis.Ticks
import           Plots.Legend
import           Plots.Themes
import           Plots.Types

-- | Where axis line for coordinate should be drawn.
data AxisLineType
  = BoxAxisLine
  | LeftAxisLine
  | MiddleAxisLine
  | RightAxisLine
  | NoAxisLine
  deriving (Show, Eq, Typeable)

instance Default AxisLineType where
  def = BoxAxisLine

-- | Information about position and style of axis lines.
data AxisLine n = AxisLine
  { _axisLineType  :: AxisLineType
  , _axisArrowOpts :: Maybe (ArrowOpts n)
  } deriving Typeable

makeLenses ''AxisLine

type AxisLines v n = v (AxisLine n)

instance Default (AxisLine n) where
  def = AxisLine
          { _axisLineType  = def
          , _axisArrowOpts = def
          }

-- Scaling

type AspectRatio v n = v n

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

data Scaling n = Scaling
  { _aspectRatio       :: Recommend n
  , _axisPostScale     :: Maybe n
  , _axisScaleMode     :: ScaleMode
  , _enlargeAxisLimits :: Maybe (Recommend n)
  }
  deriving Show

makeLenses ''Scaling

type AxisScaling v n = v (Scaling n)

instance Fractional n => Default (Scaling n) where
  def = Scaling
    { _aspectRatio       = Recommend 1
    , _axisPostScale     = Nothing
    , _axisScaleMode     = AutoScale
    , _enlargeAxisLimits = Just $ Recommend 0.1
    }

type PropertyAdjust b v n = PlotProperties b v n -> PlotProperties b v n

-- axis data type

-- | Axis is the data type that holds all the nessessary information to render
--   a plot. The idea is to use one of the default axis, customise, add plots
--   and render using @drawAxis@.
data Axis b v n = Axis
  { -- These lenses are not being exported, they're just here for instances.
    _axisAxisBounds :: Bounds v n
  , _axisPP         :: PlotProperties b v n

  -- These lenses are exported.
  , _axisColourBar  :: ColourBarOpts b n
  , _axisGridLines  :: AxisGridLines v n
  , _axisLabels     :: AxisLabels b v n
  , _axisLegend     :: Legend b n
  , _axisLines      :: AxisLines v n
  , _axisPlots      :: [Plot' b v n]
  , _axisScaling    :: AxisScaling v n
  , _axisSize       :: SizeSpec v n
  , _axisTheme      :: Theme b v n
  , _axisTickLabels :: AxisTickLabels b v n
  , _axisTicks      :: AxisTicks v n
  , _axisTitle      :: Maybe String
  , _axisScale      :: v AxisScale
  } deriving Typeable

makeLenses ''Axis

type instance V (Axis b v n) = v
type instance N (Axis b v n) = n
type instance B (Axis b v n) = b

axisLine :: E v -> Lens' (Axis b v n) (AxisLine n)
axisLine (E l) = axisLines . l

instance HasBounds (Axis b v n) where
  bounds = axisAxisBounds

-- R2 axis

instance (TypeableFloat n, Enum n, Renderable (Text n) b, Renderable (Path V2 n) b)
    => Default (Axis b V2 n) where
  def = Axis
    { _axisTitle      = Nothing
    , _axisSize       = mkWidth 300
    , _axisPlots      = []
    , _axisLegend     = def
    , _axisColourBar  = defColourBar
    , _axisTheme      = coolTheme
    , _axisAxisBounds = Bounds $ pure def
    , _axisGridLines  = pure def
    , _axisLabels     = V2 def (def & axisLabelFunction %~ (fmap . fmap $ rotateBy (1/4))
                                    & axisLabelGap .~ 40)
    , _axisScaling    = pure def
    , _axisTickLabels = pure def
    , _axisTicks      = pure def
    , _axisLines      = pure def
    , _axisScale      = pure def
    , _axisPP         = def
    }

instance HasPlotProperties (Axis b v n) where
  plotProperties = axisPP
  {-# INLINE plotProperties #-}

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

-- Drawing the axis

