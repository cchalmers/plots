{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE DeriveDataTypeable     #-}

module Plots.Axis.Labels where

import Control.Lens  hiding (( # ))
import Data.Default
import Data.Data

import Numeric

import Diagrams.Prelude   hiding (view)
import Diagrams.TwoD.Text

-- data 

-- Axis labels

data AxisLabelPosition
   = MiddleAxisLabel
   | LowerAxisLabel
   | UpperAxisLabel

data AxisLabelPlacement
   = InsideAxisLabel
   | OutsideAxisLabel

type AxisLabelFunction b n = String -> QDiagram b V2 n Any

data AxisLabel b n = AxisLabel
  { _axisLabelFunction  :: AxisLabelFunction b n
  , _axisLabelText      :: String
  , _axisLabelStyle     :: Style V2 n
  , _axisLabelGap       :: n
  , _axisLabelPos       :: AxisLabelPosition
  , _axisLabelPlacement :: AxisLabelPlacement
  }

makeLenses ''AxisLabel

instance (TypeableFloat n, Renderable (Text n) b) => Default (AxisLabel b n) where
  def = AxisLabel
          { _axisLabelFunction  = text
          , _axisLabelText      = ""
          , _axisLabelStyle     = mempty # fontSizeL 11
          , _axisLabelGap       = 12
          , _axisLabelPos       = MiddleAxisLabel
          , _axisLabelPlacement = OutsideAxisLabel
          }
                     
type AxisLabels b v n = v (AxisLabel b n)

-- Tick labels

-- | Tick labels functions are used to draw the tick labels. They has access to
--   the major ticks and the current bounds.
type TickLabelFunction b n = [n] -> (n,n) -> [(n, QDiagram b V2 n Any)]

data TickLabels b n = TickLabels
  { _tickLabelFunction :: TickLabelFunction b n
  , _tickLabelStyle    :: Style V2 n
  } deriving Typeable

makeLenses ''TickLabels

type AxisTickLabels b v n = v (TickLabels b n)

instance (TypeableFloat n, Renderable (Text n) b) => Default (TickLabels b n) where
  def = TickLabels
          { _tickLabelFunction = atMajorTicks label
          , _tickLabelStyle    = mempty # fontSizeL 9
          }

atMajorTicks :: (n -> QDiagram b V2 n Any) -> TickLabelFunction b n
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
label :: (TypeableFloat n, Renderable (Text n) b) => n -> QDiagram b V2 n Any
label n = text $ showFFloat (Just 2) n ""

leftLabel :: (TypeableFloat n, Renderable (Text n) b) => n -> QDiagram b V2 n Any
leftLabel n = alignedText 1 0.5 (showFFloat (Just 2) n "") # translateX (-7)
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

