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
module Plots.Axis where

import           Data.Default
import           Data.Monoid.Recommend
import           Data.Typeable

import           Diagrams.Coordinates.Polar
import           Diagrams.Prelude
import           Diagrams.TwoD.Text

import           Plots.Axis.ColourBar
import           Plots.Axis.Grid
import           Plots.Axis.Labels
import           Plots.Axis.Ticks
import           Plots.Legend
import           Plots.Style
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
  , _axisAxisStyle  :: AxisStyle b (BaseSpace v) n
  , _axisColourBar  :: ColourBar b n
  -- , _axisPP         :: PlotProperties b (BaseSpace v) n


  , _axisLegend     :: Legend b n
  , _axisPlots      :: [ModifiedPlot b (BaseSpace v) n]
  , _axisTitle      :: Maybe String

    -- v-wrapped
  , _axisGridLines  :: AxisGridLines v n
  , _axisLabels     :: AxisLabels b v n
  , _axisLines      :: AxisLines v n
  , _axisScaling    :: AxisScaling v n
  , _axisSize       :: SizeSpec (BaseSpace v) n
  , _axisTickLabels :: AxisTickLabels b v n
  , _axisTicks      :: AxisTicks v n
  , _axisScale      :: v AxisScale
  } deriving Typeable

makeLenses ''Axis

type instance V (Axis b v n) = BaseSpace v
type instance N (Axis b v n) = n

axisLine :: E v -> Lens' (Axis b v n) (AxisLine n)
axisLine (E l) = axisLines . l

instance HasBounds (Axis b v n) v where
  bounds = axisAxisBounds

instance HasAxisStyle (Axis b v n) b where
  axisStyle = axisAxisStyle

instance HasColourBar (Axis b v n) b where
  colourBar = axisColourBar

-- R2 axis

instance (TypeableFloat n, Enum n, Renderable (Text n) b, Renderable (Path V2 n) b)
    => Default (Axis b V2 n) where
  def = Axis
    { _axisTitle      = Nothing
    , _axisSize       = mkWidth 300
    , _axisPlots      = []
    , _axisLegend     = def
    , _axisColourBar  = defColourBar
    , _axisAxisStyle  = fadedColours
    , _axisAxisBounds = Bounds $ pure def
    , _axisGridLines  = pure def
    , _axisLabels     = V2 def (def & axisLabelFunction %~ (fmap . fmap $ rotateBy (1/4))
                                    & axisLabelGap .~ 40)
    , _axisScaling    = pure def
    , _axisTickLabels = pure def
    , _axisTicks      = pure def
    , _axisLines      = pure def
    , _axisScale      = pure def
    -- , _axisPP         = def
    }

-- instance HasPlotProperties (Axis b v n) b where
--   plotProperties = axisPP
--   {-# INLINE plotProperties #-}

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

polarAxis :: (TypeableFloat n, Enum n, Renderable (Text n) b, Renderable (Path V2 n) b) => Axis b Polar n
polarAxis = Axis
  { _axisTitle      = Nothing
  , _axisSize       = mkWidth 300
  , _axisPlots      = []
  , _axisLegend     = def
  , _axisColourBar  = defColourBar
  , _axisAxisStyle  = fadedColours
  , _axisAxisBounds = Bounds $ pure def
  , _axisGridLines  = pure def
  , _axisLabels     = Polar $ V2 def (def & axisLabelFunction %~ (fmap . fmap $ rotateBy (1/4))
                                          & axisLabelGap .~ 40
                                     )
  , _axisScaling    = pure def
  , _axisTickLabels = pure def
  , _axisTicks      = pure def
  , _axisLines      = pure def
  , _axisScale      = pure def
  -- , _axisPP         = def
  }

