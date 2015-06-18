{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

import Plots
import Plots.Axis.ColourBar
import Plots.Axis
import Diagrams.Prelude
import Diagrams.Backend.Rasterific
import Plots.Types.Heatmap
import Plots.Types (Orientation (..), orient)

myaxis :: Axis B V2 Double
myaxis = r2Axis &~ do
  addPlotable $ heatMap origin 20 20 1 (\i j -> i*i + j*j)
  axisColourBar . cbShow .= True

make :: Diagram B -> IO ()
make = renderRasterific "examples/heat.png" (mkWidth 600) . frame 20

main :: IO ()
main = make $ renderAxis myaxis

