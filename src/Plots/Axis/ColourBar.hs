{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Plots.Axis.ColourBar where

import Diagrams.Prelude
import Diagrams.TwoD.Text
import Plots.Types (Orientation (..), orient)
import Plots.Axis.Ticks
import Plots.Axis.Labels
import Plots.Themes

data ColourBarOpts b n = ColourBarOpts
  { _cbOrientation :: Orientation
  , _cbShow        :: Bool
  , _cbTickFun     :: (n,n) -> [n] -- MajorTicksFunction
  , _cbTicks       :: Bool
  , _cbTickLabels  :: [n] -> (n,n) -> TextAlignment n -> [(n, QDiagram b V2 n Any)] -- TickLabelFunction b v n
  , _cbExtent      :: V2 n
  , _cbStyle       :: Style V2 n
  -- , _colourBarSamples     :: Sampled n
  }

defColourBar :: (Renderable (Text n) b, Renderable (Path V2 n) b, TypeableFloat n, Enum n)
             => ColourBarOpts b n
defColourBar = ColourBarOpts
  { _cbOrientation = Verticle
  , _cbShow        = False
  , _cbTickFun     = linearMajorTicks 3
  , _cbTicks       = True
  , _cbTickLabels  = atMajorTicks label
  , _cbExtent      = V2 15 200
  , _cbStyle       = mempty
  -- , _colourBarSamples     :: Sampled n
  }

makeLenses ''ColourBarOpts

drawColourBar :: (TypeableFloat n, Renderable (Path V2 n) b)
              => ColourBarOpts b n -> ColourMap -> n -> n -> QDiagram b V2 n Any
drawColourBar cbo cm a b = centerY $ bar ||| strutX 5 ||| position (over (each . _1) toPos ls)
  where
    bar  = square 1 # fillTexture tx
                    # scaleX x
                    # scaleY y
                    # applyStyle (cbo ^. cbStyle)
                    # alignB
    -- ls =
    V2 x y = cbo ^. cbExtent
    toPos t = mkP2 0 (t * y / (b - a))
    tx = mkLinearGradient (toStops cm) (mkP2 0 (-0.5)) (mkP2 0 0.5) GradPad
    ps = view cbTickFun cbo (a,b)
    ls = view cbTickLabels cbo ps (a,b) tAlign
    tAlign = orient (cbo^.cbOrientation) (BoxAlignedText 0.5 1) (BoxAlignedText 0 0.5)


-- make :: Diagram PGF -> IO ()
-- make = renderPGF "examples/heatplot.pdf" (mkWidth 500) . (strutX 10 |||) . frame 30

