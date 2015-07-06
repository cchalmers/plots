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

import Diagrams.Prelude   hiding (view)
import Diagrams.TwoD.Text
import Plots.Types

-- | Function to render the axis label from a string. This is very basic
--   now and will be replace by a more sophisticated system.
type TextFunction b v n = TextAlignment n -> String -> QDiagram b v n Any

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

data AxisLabel b v n = AxisLabel
  { _axisLabelFunction  :: TextFunction b v n
  , _axisLabelText      :: String
  , _axisLabelStyle     :: Style v n
  , _axisLabelGap       :: n
  , _axisLabelPos       :: AxisLabelPosition
  , _axisLabelPlacement :: AxisLabelPlacement
  }

makeLenses ''AxisLabel

instance (TypeableFloat n, Renderable (Text n) b)
    => Default (AxisLabel b V2 n) where
  def = AxisLabel
    { _axisLabelFunction  = mkText
    , _axisLabelText      = ""
    , _axisLabelStyle     = mempty & fontSize (output 8)
    , _axisLabelGap       = 20
    , _axisLabelPos       = MiddleAxisLabel
    , _axisLabelPlacement = OutsideAxisLabel
    }

type AxisLabels b v n = v (AxisLabel b (BaseSpace v) n)

------------------------------------------------------------------------
-- Tick labels
------------------------------------------------------------------------

-- Labels that are placed next to the ticks (usually) of an axis.

-- | Tick labels functions are used to draw the tick labels. They has access to
--   the major ticks and the current bounds. Returns the position of the
--   tick and label to use at that position.
type TickLabelFunction n = [n] -> (n,n) -> [(n, String)]

data TickLabels b v n = TickLabels
  { _tickLabelFun      :: TickLabelFunction n
  , _tickLabelTextFun  :: TextFunction b v n
  , _tickLabelStyle    :: Style v n
  , _tickGap           :: n
  } deriving Typeable

makeLenses ''TickLabels

type AxisTickLabels b v n = v (TickLabels b (BaseSpace v) n)

instance (TypeableFloat n, Renderable (Text n) b)
    => Default (TickLabels b V2 n) where
  def = TickLabels
    { _tickLabelFun     = atMajorTicks floatShow
    , _tickLabelTextFun = mkText
    , _tickLabelStyle   = mempty & fontSize (output 8)
    , _tickGap          = 8
    }

-- | Numbers are shown as 'Float's to reduce the chance of numbers like
--   1.30000000008. (This is not an idea solution.)
floatShow :: Real n => n -> String
floatShow = show . (realToFrac :: Real n => n -> Float)

-- | Make a 'TickLabelFunction' by specifying how to draw a single label
--   from a position on the axis.
atMajorTicks :: (n -> String) -> TickLabelFunction n
atMajorTicks f ticks _ = map ((,) <*> f) ticks

-- | Use the list of strings as the labels for the axis, starting at 1
-- and going to 2, 3, 4 ...
stringLabels :: Num n => [String] -> TickLabelFunction n
stringLabels nms _ _ = imap (\i l -> (fromIntegral (i + 1), l)) nms

