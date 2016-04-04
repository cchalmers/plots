{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

import Plots
import Plots.Axis.ColourBar
import Plots.Axis
import Diagrams.Prelude
import Diagrams.Backend.Rasterific
import Plots.Types.HeatMap
import Plots.Types (Orientation (..), orient)

myaxis :: Axis B V2 Double
myaxis = r2Axis &~ do
  heatMap (V2 20 20) (\(V2 i j) -> fromIntegral $ i*i + j*j) $ pure ()
  -- addPlotable $ heatMap origin 20 20 1
  colourBarVisible .= True
  -- axisColourBar . cbShow .= True

make :: Diagram B -> IO ()
make = renderRasterific "examples/heat.png" (mkWidth 600) . frame 20

main :: IO ()
main = make $ renderAxis myaxis

