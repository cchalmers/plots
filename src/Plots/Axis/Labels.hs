{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE DeriveDataTypeable     #-}

module Plots.Axis.Labels where

import Control.Lens  hiding (( # ))
import Data.Default
import Data.Data

import Numeric

import Diagrams.Prelude   hiding (view)
import Diagrams.TwoD.Text

------------------------------------------------------------------------
-- Axis labels
------------------------------------------------------------------------

-- Labels for the axis. Pretty basic right now.

data AxisLabelPosition
   = MiddleAxisLabel
   | LowerAxisLabel
   | UpperAxisLabel

data AxisLabelPlacement
   = InsideAxisLabel
   | OutsideAxisLabel

-- | Function to render the axis label from a string. This is very basic
-- now and will be replace by a more sophisticated system.
type AxisLabelFunction b v n = String -> QDiagram b v n Any

data AxisLabel b v n = AxisLabel
  { _axisLabelFunction  :: AxisLabelFunction b v n
  , _axisLabelText      :: String
  , _axisLabelStyle     :: Style v n
  , _axisLabelGap       :: n
  , _axisLabelPos       :: AxisLabelPosition
  , _axisLabelPlacement :: AxisLabelPlacement
  }

makeLenses ''AxisLabel

instance (TypeableFloat n, Renderable (Text n) b) => Default (AxisLabel b V2 n) where
  def = AxisLabel
          { _axisLabelFunction  = text
          , _axisLabelText      = ""
          , _axisLabelStyle     = mempty # fontSizeL 11
          , _axisLabelGap       = 12
          , _axisLabelPos       = MiddleAxisLabel
          , _axisLabelPlacement = OutsideAxisLabel
          }

type AxisLabels b v n = v (AxisLabel b v n)

------------------------------------------------------------------------
-- Tick labels
------------------------------------------------------------------------

-- Labels that are placed next to the ticks (usually) of an axis.

-- | Tick labels functions are used to draw the tick labels. They has access to
--   the major ticks and the current bounds. Returns the position of the
--   tick and label to use at that position.
type TickLabelFunction b v n = [n] -> (n,n) -> [(n, QDiagram b v n Any)]

data TickLabels b v n = TickLabels
  { _tickLabelFunction :: TickLabelFunction b v n
  , _tickLabelStyle    :: Style v n
  } deriving Typeable

makeLenses ''TickLabels

type AxisTickLabels b v n = v (TickLabels b v n)

instance (TypeableFloat n, Renderable (Text n) b) => Default (TickLabels b V2 n) where
  def = TickLabels
          { _tickLabelFunction = atMajorTicks label
          , _tickLabelStyle    = mempty # fontSizeL 9
          }

-- | Make a 'TickLabelFunction' by specifying how to draw a single label
--   from a position on the axis.
atMajorTicks :: (n -> QDiagram b v n Any) -> TickLabelFunction b v n
atMajorTicks f ticks _ = map ((,) <*> f) ticks

-- | Standard way to render a label by using 'Text'.
label :: (TypeableFloat n, Renderable (Text n) b) => n -> QDiagram b V2 n Any
label n = text $ showFFloat (Just 2) n ""

leftLabel :: (TypeableFloat n, Renderable (Text n) b) => n -> QDiagram b V2 n Any
leftLabel n = alignedText 1 0.5 (showFFloat (Just 2) n "") # translateX (-7)

-- | Use the list of strings as the labels for the axis, starting at 0
-- and going to 1, 2, 3 ...
stringLabels :: Num n => (String -> QDiagram b v n Any) -> [String] -> TickLabelFunction b v n
stringLabels f ls _ _ = imap (\i l -> (fromIntegral i, f l)) ls


-- -- horrible name
-- labelFunctionFromTicks :: (Double -> Diagram b R2) -> TickFunction -> LabelFunction b
-- labelFunctionFromTickFunction f aF bounds = map ((,) <*> f) (aF bounds)

