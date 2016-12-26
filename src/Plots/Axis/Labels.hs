{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Plots.Axis.Labels
-- Copyright   :  (C) 2015-2017 Christopher Chalmers
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Christopher Chalmers
-- Stability   :  experimental
-- Portability :  non-portable
--
-- There are two kinds of labels this module deals with: The 'AxisLabel'
-- labels are placed next to an axis line. The 'TickLabels' are the
-- numbers (usually) next to each major tick on an axis line.
--
----------------------------------------------------------------------------
module Plots.Axis.Labels
  ( -- * Axis line labels
    HasAxisLabel (..)
  , AxisLabel
  , AxisLabelPosition (..)
  , AxisLabelPlacement (..)

    -- * Axis tick labels
  , TickLabels
  , HasTickLabels (..)
  , tickLabelPositions
  , atMajorTicks

    -- * Misc
  , TextFunction
  ) where

import           Control.Lens       hiding (( # ))
import           Data.Data
import           Data.Default

import           Diagrams.Prelude   hiding (view)
import           Diagrams.TwoD.Text
import           Plots.Types

-- | Function to render the axis label from a string. This is very basic
--   now and will be replace by a more sophisticated system.
type TextFunction v = TextAlignment Double -> String -> Diagram v

------------------------------------------------------------------------
-- Axis labels
------------------------------------------------------------------------

-- | The position of the 'AxisLabel' along the axis.
data AxisLabelPosition
   = MiddleAxisLabel
   | LowerAxisLabel
   | UpperAxisLabel

-- | Whether the 'AxisLabel' should be inside or outside the axis.
data AxisLabelPlacement
   = InsideAxisLabel
   | OutsideAxisLabel

-- | 'AxisLabel' describes the label next to each axis line. They are
--   normally set with the 'Plots.Axis.xLabel' and 'Plots.Axis.yLabel'
--   helper function:
--
-- @
-- myAxis = r2Axis &~ do
--   'Plots.Axis.xLabel' .= "time (s)"
--   'Plots.Axis.yLabel' .= "height (m)"
-- @
--
--   See 'HasAxisLabel' for more advanced settings.
data AxisLabel v = AxisLabel
  { alFun       :: TextFunction v
  , alText      :: String
  , alStyle     :: Style v Double
  , alGap       :: Double
  , alPos       :: AxisLabelPosition
  , alPlacement :: AxisLabelPlacement
  , alVisible   :: Bool
  }

type instance V (AxisLabel v) = v
type instance N (AxisLabel v) = Double

class HasAxisLabel f a where
  -- | The options for the label of the axis. This can be used on
  --   various levels of the axis:
  --
  -- @
  -- 'axisLabel' :: 'Traversal'' ('Axis' b c n)       ('AxisLabel' ('BaseSpace' c) n)
  -- 'axisLabel' :: 'Lens''      ('SingleAxis' v) ('AxisLabel' v n)
  -- 'axisLabel' :: 'Lens''      ('AxisLabel' v n)    ('AxisLabel' v n)
  -- @
  axisLabel :: LensLike' f a (AxisLabel (V a))

  -- | The text to use when labeling the axis.
  axisLabelText :: Functor f => LensLike' f a String
  axisLabelText = axisLabel . lens alText (\al txt -> al {alText = txt})

  -- | The 'TextFunction' to render the text of the axis label.
  axisLabelTextFunction :: Functor f => LensLike' f a (TextFunction (V a))
  axisLabelTextFunction = axisLabel . lens alFun (\al f -> al {alFun = f})

  -- | The gap between the axis and the labels, in the direction
  --   corresponding to the 'axisLabelPosition'.
  axisLabelGap :: Functor f => LensLike' f a Double
  axisLabelGap = axisLabel . lens alGap (\al sty -> al {alGap = sty})

  -- | The 'Style' to use on the rendered text.
  axisLabelStyle :: Functor f => LensLike' f a (Style (V a) Double)
  axisLabelStyle = axisLabel . lens alStyle (\al sty -> al {alStyle = sty})

  -- | The position the label will be placed parallel to the axis.
  axisLabelPosition :: Functor f => LensLike' f a AxisLabelPosition
  axisLabelPosition = axisLabel . lens alPos (\al sty -> al {alPos = sty})

  -- | Whether the axis label should be placed inside or outside the
  --   axis.
  axisLabelPlacement :: Functor f => LensLike' f a AxisLabelPosition
  axisLabelPlacement = axisLabel . lens alPos (\al sty -> al {alPos = sty})

instance HasAxisLabel f (AxisLabel v) where
  axisLabel = id

instance ApplyStyle (AxisLabel v)
instance HasStyle (AxisLabel v) where
  style = axisLabelStyle

instance HasVisibility (AxisLabel v) where
  visible = lens alVisible (\al b -> al {alVisible = b})

instance HasGap (AxisLabel v) where
  gap = axisLabelGap

instance Default (AxisLabel V2) where
  def = AxisLabel
    { alFun       = mkText
    , alText      = ""
    , alStyle     = mempty & fontSize (output 11)
                           & backupFillColor black
    , alGap       = 30
    , alPos       = MiddleAxisLabel
    , alPlacement = OutsideAxisLabel
    , alVisible   = True
    }

------------------------------------------------------------------------
-- Tick labels
------------------------------------------------------------------------

-- | 'TickLabels' describes how to draw the labels next to ticks. See
--   'HasTickLabels' for more options.
data TickLabels v = TickLabels
  { tlFun     :: [Double] -> (Double,Double) -> [(Double, String)]
  , tlTextFun :: TextFunction v
  , tlStyle   :: Style v Double
  , tlGap     :: Double
  , tlVisible :: Bool
  } deriving Typeable

type instance V (TickLabels v) = v
type instance N (TickLabels v) = Double

class HasTickLabels f a where
  -- | The options for the label of ticks. This can be used on various
  --   levels of the axis:
  --
  -- @
  -- 'tickLabel' :: 'Traversal'' ('Tick' c)       ('TickLabels' ('BaseSpace' c))
  -- 'tickLabel' :: 'Lens''      ('SingleAxis' v) ('TickLabels' v)
  -- 'tickLabel' :: 'Lens''      ('TickLabel' v)  ('TickLabels' v)
  -- @
  tickLabel :: LensLike' f a (TickLabels (V a))

  -- | The 'TextFunction' to render the text.
  --
  --   'Default' is 'mkText'.
  tickLabelTextFunction :: Functor f => LensLike' f a (TextFunction (V a))
  tickLabelTextFunction = tickLabel . lens tlTextFun (\tl f -> tl {tlTextFun = f})

  -- | Tick labels functions are used to draw the tick labels. They have
  --   access to the major ticks and the current bounds. Returns the
  --   position of the tick and label to use at that position.
  --
  --   'Default' is @'atMajorTicks' 'floatShow'@
  tickLabelFunction :: Functor f => LensLike' f a ([Double] -> (Double, Double) -> [(Double, String)])
  tickLabelFunction = tickLabel . lens tlFun (\tl f -> tl {tlFun = f})

  -- | The 'Style' to use on the rendered text.
  --
  --   'Default' is @'fontSize' ('output' 11)@.
  tickLabelStyle :: Functor f => LensLike' f a (Style (V a) Double)
  tickLabelStyle = tickLabel . lens tlStyle (\tl sty -> tl {tlStyle = sty})

  -- | The gap between the axis and the tick labels.
  --
  --   'Default' is @12@.
  tickLabelGap :: Functor f => LensLike' f a Double
  tickLabelGap = tickLabel . lens tlGap (\tl n -> tl {tlGap = n})

instance HasTickLabels f (TickLabels v) where
  tickLabel = id

instance HasGap (TickLabels v) where
  gap = tickLabelGap

instance Default (TickLabels V2) where
  def = TickLabels
    { tlFun     = atMajorTicks floatShow
    , tlTextFun = mkText
    , tlStyle   = mempty & fontSize (output 11)
                         & backupFillColor black
    , tlGap     = 12
    , tlVisible = True
    }

instance HasVisibility (TickLabels v) where
  visible = lens tlVisible (\tl b -> tl {tlVisible = b})

-- | Setter over the final positions the major ticks. This is not as
--   general as 'tickLabelFunction' because you don't have access to the
--   bounds but it can be useful when you know exactly what ticks you
--   want to add or modify existing tick positions or to add an extra
--   value:
--
-- @
-- xAxis . tickLabelPositions .= [(1, "apples"), (2, "oranges"), (3, "bananas"]
-- yAxis . tickLabelPositions <>= [(1.5, "critial mass")]
-- @
--
--  If you want to change or add normal ticks see 'majorTicksFunction'.
--
tickLabelPositions
  :: (HasTickLabels f a, Settable f) => LensLike' f a [(Double, String)]
tickLabelPositions = tickLabelFunction . mapped . mapped

-- | Numbers are shown as 'Float's to reduce the chance of numbers like
--   1.30000000008. (This is not an ideal solution.)
floatShow :: Real n => n -> String
floatShow = show . (realToFrac :: Real n => n -> Float)

-- | Make a 'TickLabelFunction' by specifying how to draw a single label
--   from a position on the axis.
atMajorTicks :: (Double -> String) -> [Double] -> (Double,Double) -> [(Double, String)]
atMajorTicks f ticks _ = map ((,) <*> f) ticks

-- -- | Use the list of strings as the labels for the axis, starting at 1
-- --   and going to 2, 3, 4 ... .
-- stringLabels :: Num n => [(n, String)] -> TickLabelFunction n
-- stringLabels nms _ _ = iover (each . itraversed) (\i l -> (fromIntegral (i + 1), l)) nms

