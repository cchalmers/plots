{-# LANGUAGE FlexibleContexts #-}
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
-- Low level module defining type for the axis colour bar.
--
----------------------------------------------------------------------------
module Plots.Axis.ColourBar where

import Diagrams.Prelude
import Diagrams.TwoD.Text
import Plots.Types (Orientation (..), orient)
import Plots.Axis.Ticks
import Plots.Axis.Labels
import Plots.Themes
import Plots.Legend

data ColourBarOpts b n = ColourBarOpts
  { _cbOrientation :: Orientation
  , _cbShow        :: Bool
  , _cbTickFun     :: (n,n) -> [n] -- MajorTicksFunction
  , _cbTicks       :: Bool
  , _cbTickLabels  :: [n] -> (n,n) -> [(n, String)]
  , _cbTextFun     :: TextAlignment n -> String -> QDiagram b V2 n Any
  , _cbExtent      :: V2 n
  , _cbGap         :: n
  , _cbStyle       :: Style V2 n
  -- , _colourBarSamples     :: Sampled n
  }

defColourBar :: (Renderable (Text n) b, Renderable (Path V2 n) b, TypeableFloat n, Enum n)
             => ColourBarOpts b n
defColourBar = ColourBarOpts
  { _cbOrientation = Vertical
  , _cbShow        = False
  , _cbTextFun     = mkText
  , _cbTickFun     = linearMajorTicks 3
  , _cbTicks       = True
  , _cbTickLabels  = atMajorTicks floatShow
  , _cbExtent      = V2 15 200
  , _cbGap         = 20
  , _cbStyle       = mempty
  -- , _colourBarSamples     :: Sampled n
  }

makeLenses ''ColourBarOpts

drawColourBar :: (TypeableFloat n, Renderable (Path V2 n) b)
              => ColourBarOpts b n -> ColourMap -> n -> n -> QDiagram b V2 n Any
drawColourBar cbo cm a b = centerY $ bar ||| strutX 5 ||| labels
  where
    bar  = square 1 # fillTexture tx
                    # scaleX x
                    # scaleY y
                    # applyStyle (cbo ^. cbStyle)
                    # alignB
    labels = position (map (bimap toPos (view cbTextFun cbo tAlign)) ls)
               # fontSizeO 8
    V2 x y = cbo ^. cbExtent
    toPos t = mkP2 0 (t * y / (b - a))
    tx = mkLinearGradient (toStops cm) (mkP2 0 (-0.5)) (mkP2 0 0.5) GradPad
    ps = view cbTickFun cbo (a,b)
    ls = view cbTickLabels cbo ps (a,b)
    tAlign = orient (cbo^.cbOrientation) (BoxAlignedText 0.5 1) (BoxAlignedText 0 0.5)

addColourBar :: (TypeableFloat n, Renderable (Path V2 n) b)
             => BoundingBox V2 n
             -> ColourBarOpts b n
             -> ColourMap
             -> n
             -> n
             -> QDiagram b V2 n Any
addColourBar bb cbo cm a b
  | not $ cbo^.cbShow = mempty
  | otherwise         = alignTo cbPos bb cbAnchor v cb
  where
    cbPos    = orient (cbo^.cbOrientation) South     East
    cbAnchor = orient (cbo^.cbOrientation) AnchorTop AnchorLeft
    v        = (cbo^.cbGap) *^ orient (cbo^.cbOrientation) unit_Y unitX
    cb       = drawColourBar cbo cm a b
