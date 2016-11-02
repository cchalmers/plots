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
-- Copyright   :  (C) 2015 Christopher Chalmers
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Christopher Chalmers
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Low level module defining types for axis labels and tick labels.
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
type TextFunction b v n = TextAlignment n -> String -> QDiagram b v n Any

------------------------------------------------------------------------
-- Axis labels
------------------------------------------------------------------------

-- | The position of the 'AxisLabel' along the axis.
data AxisLabelPosition
   = MiddleAxisLabel
   | LowerAxisLabel
   | UpperAxisLabel

-- | Whether the 'AxisLabel' should be inside or ouside the axis.
data AxisLabelPlacement
   = InsideAxisLabel
   | OutsideAxisLabel

data AxisLabel b v n = AxisLabel
  { alFun       :: TextFunction b v n
  , alText      :: String
  , alStyle     :: Style v n
  , alGap       :: n
  , alPos       :: AxisLabelPosition
  , alPlacement :: AxisLabelPlacement
  , alVisible   :: Bool
  }

type instance V (AxisLabel b v n) = v
type instance N (AxisLabel b v n) = n

class HasAxisLabel f a b | a -> b where
  -- | The options for the label of the axis. This can be used on
  --   various levels of the axis:
  --
  -- @
  -- 'axisLabel' :: 'Traversal'' ('Axis' b c n)       ('AxisLabel' ('BaseSpace' c) n)
  -- 'axisLabel' :: 'Lens''      ('SingleAxis' b v n) ('AxisLabel' v n)
  -- 'axisLabel' :: 'Lens''      ('AxisLabel' v n)    ('AxisLabel' v n)
  -- @
  axisLabel :: LensLike' f a (AxisLabel b (V a) (N a))

  -- | The text to use when labeling the axis.
  axisLabelText :: Functor f => LensLike' f a String
  axisLabelText = axisLabel . lens alText (\al txt -> al {alText = txt})

  -- | The 'TextFunction' to render the text of the axis label.
  axisLabelTextFunction :: Functor f => LensLike' f a (TextFunction b (V a) (N a))
  axisLabelTextFunction = axisLabel . lens alFun (\al f -> al {alFun = f})

  -- | The gap between the axis and the labels, in the direction
  --   corresponding to the 'axisLabelPosition'.
  axisLabelGap :: Functor f => LensLike' f a (N a)
  axisLabelGap = axisLabel . lens alGap (\al sty -> al {alGap = sty})

  -- | The 'Style' to use on the rendered text.
  axisLabelStyle :: Functor f => LensLike' f a (Style (V a) (N a))
  axisLabelStyle = axisLabel . lens alStyle (\al sty -> al {alStyle = sty})

  -- | The position the label will be placed parallel the axis.
  axisLabelPosition :: Functor f => LensLike' f a AxisLabelPosition
  axisLabelPosition = axisLabel . lens alPos (\al sty -> al {alPos = sty})

  -- | Whether the axis label should be placed inside or outside the
  --   axis.
  axisLabelPlacement :: Functor f => LensLike' f a AxisLabelPosition
  axisLabelPlacement = axisLabel . lens alPos (\al sty -> al {alPos = sty})

instance HasAxisLabel f (AxisLabel b v n) b where
  axisLabel = id

instance Typeable n => HasStyle (AxisLabel b v n) where
  applyStyle = over axisLabelStyle . applyStyle

instance HasVisibility (AxisLabel b v n) where
  visible = lens alVisible (\al b -> al {alVisible = b})

instance (TypeableFloat n, Renderable (Text n) b)
    => Default (AxisLabel b V2 n) where
  def = AxisLabel
    { alFun       = mkText
    , alText      = ""
    , alStyle     = mempty & fontSize (output 11)
    , alGap       = 30
    , alPos       = MiddleAxisLabel
    , alPlacement = OutsideAxisLabel
    , alVisible   = True
    }

------------------------------------------------------------------------
-- Tick labels
------------------------------------------------------------------------

-- Labels that are placed next to the ticks (usually) of an axis.

-- | Tick labels functions are used to draw the tick labels. They has access to
--   the major ticks and the current bounds. Returns the position of the
--   tick and label to use at that position.
type TickLabelFunction n = [n] -> (n,n) -> [(n, String)]

data TickLabels b v n = TickLabels
  { tlFun     :: TickLabelFunction n
  , tlTextFun :: TextFunction b v n
  , tlStyle   :: Style v n
  , tlGap     :: n
  , tlVisible :: Bool
  } deriving Typeable

type instance V (TickLabels b v n) = v
type instance N (TickLabels b v n) = n

class HasTickLabels f a b | a -> b where
  -- | The options for the label of ticks. This can be used on various
  --   levels of the axis:
  --
  -- @
  -- 'tickLabel' :: 'Traversal'' ('Tick' b c n)       ('TickLabels' ('BaseSpace' c) n)
  -- 'tickLabel' :: 'Lens''      ('SingleAxis' b v n) ('TickLabels' v n)
  -- 'tickLabel' :: 'Lens''      ('TickLabel' v n)    ('TickLabels' v n)
  -- @
  tickLabel :: LensLike' f a (TickLabels b (V a) (N a))

  -- | The 'TextFunction' to render the text.
  --
  --   'Default' is 'mkText'.
  tickLabelTextFunction :: Functor f => LensLike' f a (TextFunction b (V a) (N a))
  tickLabelTextFunction = tickLabel . lens tlTextFun (\tl f -> tl {tlTextFun = f})

  -- | The 'TextFunction' to render the text.
  --
  --   'Default' is @'atMajorTicks' 'floatShow'@
  tickLabelFunction :: Functor f => LensLike' f a (TickLabelFunction (N a))
  tickLabelFunction = tickLabel . lens tlFun (\tl f -> tl {tlFun = f})

  -- | The 'Style' to use on the rendered text.
  --
  --   'Default' is @'fontSize' ('output' 11)@.
  tickLabelStyle :: Functor f => LensLike' f a (Style (V a) (N a))
  tickLabelStyle = tickLabel . lens tlStyle (\tl sty -> tl {tlStyle = sty})

  -- | The gap between the axis and the tick labels.
  --
  --   'Default' is @12@.
  tickLabelGap :: Functor f => LensLike' f a (N a)
  tickLabelGap = tickLabel . lens tlGap (\tl n -> tl {tlGap = n})

instance HasTickLabels f (TickLabels b v n) b where
  tickLabel = id

instance (TypeableFloat n, Renderable (Text n) b)
    => Default (TickLabels b V2 n) where
  def = TickLabels
    { tlFun     = atMajorTicks floatShow
    , tlTextFun = mkText
    , tlStyle   = mempty & fontSize (output 11)
    , tlGap     = 12
    , tlVisible = True
    }

instance HasVisibility (TickLabels b v n) where
  visible = lens tlVisible (\tl b -> tl {tlVisible = b})

-- | Setter over the final positions the major ticks. This is not as
--   general as 'minorTicksFunction' because you don't have access to
--   the bounds but it can be useful when you know exactly what ticks
--   you want to add or modify existing tick positions.
tickLabelPositions
  :: (HasTickLabels f a b, Settable f)
  => LensLike' f a [(N a, String)]
tickLabelPositions = tickLabelFunction . mapped . mapped

-- | Numbers are shown as 'Float's to reduce the chance of numbers like
--   1.30000000008. (This is not an ideal solution.)
floatShow :: Real n => n -> String
floatShow = show . (realToFrac :: Real n => n -> Float)

-- | Make a 'TickLabelFunction' by specifying how to draw a single label
--   from a position on the axis.
atMajorTicks :: (n -> String) -> TickLabelFunction n
atMajorTicks f ticks _ = map ((,) <*> f) ticks

-- -- | Use the list of strings as the labels for the axis, starting at 1
-- --   and going to 2, 3, 4 ... .
-- stringLabels :: Num n => [(n, String)] -> TickLabelFunction n
-- stringLabels nms _ _ = iover (each . itraversed) (\i l -> (fromIntegral (i + 1), l)) nms

