{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE DeriveDataTypeable     #-}

module Plots.Axis.Labels where

import Control.Lens  hiding (( # ))
import Data.Default
import Data.Typeable
import Text.Printf

import Diagrams.Prelude   hiding (view)
import Diagrams.TwoD.Text

import Diagrams.Coordinates.Traversals

-- data 

-- Axis labels

data AxisLabelPosition
   = MiddleAxisLabel
   | LowerAxisLabel
   | UpperAxisLabel

data AxisLabelPlacement
   = InsideAxisLabel
   | OutsideAxisLabel

type AxisLabelFunction b = String -> Diagram b R2

data AxisLabel b = AxisLabel
  { _axisLabelFunction  :: AxisLabelFunction b
  , _axisLabelText      :: String
  , _axisLabelStyle     :: Style R2
  , _axisLabelGap       :: Double
  , _axisLabelPos       :: AxisLabelPosition
  , _axisLabelPlacement :: AxisLabelPlacement
  }

makeLenses ''AxisLabel

instance Renderable Text b => Default (AxisLabel b) where
  def = AxisLabel
          { _axisLabelFunction  = text
          , _axisLabelText      = ""
          , _axisLabelStyle     = mempty # fontSizeL 11
          , _axisLabelGap       = 12
          , _axisLabelPos       = MiddleAxisLabel
          , _axisLabelPlacement = OutsideAxisLabel
          }
                     
type AxisLabels b v = T v (AxisLabel b)

-- Tick labels

-- | Tick labels functions are used to draw the tick labels. They has access to
--   the major ticks and the current bounds.
type TickLabelFunction b = [Double] -> (Double,Double) -> [(Double, Diagram b R2)]

data TickLabels b = TickLabels
  { _tickLabelFunction :: TickLabelFunction b
  , _tickLabelStyle    :: Style R2
  } deriving Typeable

makeLenses ''TickLabels

type AxisTickLabels b v = T v (TickLabels b)

instance Renderable Text b => Default (TickLabels b) where
  def = TickLabels
          { _tickLabelFunction = atMajorTicks label
          , _tickLabelStyle    = mempty # fontSizeL 9
          }

atMajorTicks :: (Double -> Diagram b R2) -> TickLabelFunction b
atMajorTicks f ticks _ = map ((,) <*> f) ticks

-- instance Renderable Text b => Default (TickLabels b) where
--   def = labelsFromTicks (def :: AxisTicks)

-- Lenes onto both x and y components

-- labels :: HasAxisLabels a b => Traversal' a (LabelFunction b)
-- labels = traverse2lens xLabels yLabels

-- labelsFromTicks :: (Renderable Text b, HasAxisTicks a) => a -> AxisTickLabel b
-- labelsFromTicks a = AxisLabels (a ^. xMajorTicks . to fx) (a ^. yMajorTicks . to fy)
--   where
--   fx = labelFunctionFromTickFunction (\n -> alignedText 0.5 1 (printf "%.1f" n) # translateY (-7))
--   fy = labelFunctionFromTickFunction (\n -> alignedText 1 0.5 (printf "%.1f" n) # translateX (-7))
--
label :: (Renderable Text b) => Double -> Diagram b R2
label n = text (printf "%.1f" n)

leftLabel :: (Renderable Text b) => Double -> Diagram b R2
leftLabel n = alignedText 1 0.5 (printf "%.1f" n) # translateX (-7)
--
-- rightLabel :: (Renderable Text b) => Double -> Diagram b R2
-- rightLabel n = alignedText 0 0.5 (printf "%.1f" n) # translateX 7
--
-- bottomLabel :: (Renderable Text b) => Double -> Diagram b R2
-- bottomLabel n = alignedText 0.5 1 (printf "%.1f" n) # translateY (-7)
--
-- topLabel :: (Renderable Text b) => Double -> Diagram b R2
-- topLabel n = alignedText 0.5 0 (printf "%.1f" n) # translateY 7




-- -- horrible name
-- labelFunctionFromTicks :: (Double -> Diagram b R2) -> TickFunction -> LabelFunction b
-- labelFunctionFromTickFunction f aF bounds = map ((,) <*> f) (aF bounds)

