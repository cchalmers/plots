{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Plots.Axis.ColourBar
-- Copyright   :  (C) 2015 Christopher Chalmers
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Christopher Chalmers
-- Stability   :  experimental
-- Portability :  non-portable
--
-- The colour bar is a way of visualising the 'ColourMap'.
--
----------------------------------------------------------------------------
module Plots.Axis.ColourBar where

import Diagrams.Prelude
import Diagrams.TwoD.Text
import Plots.Axis.Ticks
import Plots.Axis.Labels
import Plots.Style
import Plots.Legend
import Plots.Types
import Data.Typeable

-- | Options for drawing a colour bar. Note that for an axis, the
--   'ColourMap' is stored in the 'AxisStyle'. These options are for
--   other aspects of the bar, not the colours used.
data ColourBar b n = ColourBar
  { cbOrientation :: Orientation
  , cbVisible     :: Bool
  , cbTickFun     :: (n,n) -> [n] -- MajorTicksFunction
  , cbTickVisible :: Bool
  , cbTickLabels  :: [n] -> (n,n) -> [(n, String)]
  , cbTickAlign   :: TickAlign
  , cbTextFun     :: TextAlignment n -> String -> QDiagram b V2 n Any
  , cbExtent      :: V2 n
  , cbGap         :: n
  , cbStyle       :: Style V2 n
  }

type instance V (ColourBar b n) = V2
type instance N (ColourBar b n) = n

defColourBar :: (Renderable (Text n) b, Renderable (Path V2 n) b, TypeableFloat n, Enum n)
             => ColourBar b n
defColourBar = ColourBar
  { cbOrientation = Vertical
  , cbVisible     = False
  , cbTextFun     = mkText
  , cbTickFun     = linearMajorTicks 3
  , cbTickVisible = True
  , cbTickAlign   = centreTicks
  , cbTickLabels  = atMajorTicks floatShow
  , cbExtent      = V2 15 200
  , cbGap         = 20
  , cbStyle       = mempty
  -- , _colourBarSamples     :: Sampled n
  }


class HasColourBar a b | a -> b where
  -- | Lens onto the 'ColourBar'.
  colourBar :: Lens' a (ColourBar b (N a))

  -- | Whether the colour bar should be visible when rendering.
  colourBarVisible :: Lens' a Bool
  colourBarVisible = colourBar . lens cbVisible (\c a -> c {cbVisible = a})

  -- | Placement of ticks given the range of the colour bar.
  colourBarTickFun :: Lens' a ((N a, N a) -> [N a])
  colourBarTickFun = colourBar . lens cbTickFun (\c a -> c {cbTickFun = a})

  -- | Alignment of ticks for the colour bar.
  colourBarTickAlign :: Lens' a TickAlign
  colourBarTickAlign = colourBar . lens cbTickAlign (\c a -> c {cbTickAlign = a})

  -- | Whether to show any ticks in the colour bar.
  colourBarTickVisible :: Lens' a Bool
  colourBarTickVisible = colourBar . lens cbTickVisible (\c a -> c {cbTickVisible = a})

  -- | Placement of tick labels given the tick positions and bounds for
  --   the colour bar.
  colourBarTickLabels :: Lens' a ([N a] -> (N a, N a) -> [(N a, String)])
  colourBarTickLabels = colourBar . lens cbTickLabels (\c a -> c {cbTickLabels = a})

  -- | The text rendering function used for the tick labels of the colour bar.
  colourBarTextFun :: Lens' a (TextAlignment (N a) -> String -> QDiagram b V2 (N a) Any)
  colourBarTextFun = colourBar . lens cbTextFun (\c a -> c {cbTextFun = a})

  -- | The size of the colour bar when it's in a 'Horizontal' 'orientation'.
  colourBarExtent :: Lens' a (V2 (N a))
  colourBarExtent = colourBar . lens cbExtent (\c a -> c {cbExtent = a})

  -- | Gap between the axis and the colour bar (if rendered with an axis).
  colourBarGap :: Lens' a (N a)
  colourBarGap = colourBar . lens cbGap (\c a -> c {cbGap = a})

  -- | Style used for the outline of a colour bar.
  colourBarStyle :: Lens' a (Style V2 (N a))
  colourBarStyle = colourBar . lens cbStyle (\c a -> c {cbStyle = a})

instance HasColourBar (ColourBar b n) b where
  colourBar = id

instance HasOrientation (ColourBar b n) where
  orientation = lens cbOrientation (\c a -> c {cbOrientation = a})

instance Typeable n => HasStyle (ColourBar b n) where
  applyStyle sty = colourBarStyle %~ applyStyle sty

-- | Draw a standalone colour bar.
drawColourBar
  :: (TypeableFloat n, Renderable (Path V2 n) b)
  => ColourBar b n       -- ^ options
  -> ColourMap           -- ^ colours to use
  -> (n,n)               -- ^ bounds for colour bar tick labels
  -> QDiagram b V2 n Any -- ^ resulting colour bar
drawColourBar cbo cm (a,b) = centerY $ bar ||| strutX 5 ||| labels
  where
    bar  = square 1 # fillTexture tx
                    # scaleX x
                    # scaleY y
                    # applyStyle (cbStyle cbo)
                    # alignB
    labels = position (map (bimap toPos (cbTextFun cbo tAlign)) ls)
               # fontSizeO 8
    V2 x y = cbExtent cbo
    toPos t = mkP2 0 (t * y / (b - a))
    tx = mkLinearGradient (toStops cm) (mkP2 0 (-0.5)) (mkP2 0 0.5) GradPad
    ps = cbTickFun cbo (a,b)
    ls = cbTickLabels cbo ps (a,b)
    tAlign = orient cbo (BoxAlignedText 0.5 1) (BoxAlignedText 0 0.5)

-- | Add a colour bar to an object, using the bounding box for the object.
addColourBar
  :: (TypeableFloat n, Renderable (Path V2 n) b)
  => BoundingBox V2 n
  -> ColourBar b n
  -> ColourMap
  -> (n,n)
  -> QDiagram b V2 n Any
addColourBar bb cbo cm ab
  | cbVisible cbo = alignTo cbPos bb cbAnchor v cb
  | otherwise     = mempty
  where
    cbPos    = orient cbo South     East
    cbAnchor = orient cbo AnchorTop AnchorLeft
    v        = cbGap cbo *^ orient cbo unit_Y unitX
    cb       = drawColourBar cbo cm ab
