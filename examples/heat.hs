{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

import Plots
import Diagrams.Prelude
import Diagrams.Backend.Rasterific.CmdLine
import Plots.Types.HeatMap

myaxis :: Axis B V2 Double
myaxis = r2Axis &~ do
  heatMap (V2 20 20) (\(V2 i j) -> fromIntegral $ i*i + j*j) $ pure ()
  -- addPlotable $ heatMap origin 20 20 1
  colourBarVisible .= True
  -- axisColourBar . cbShow .= True

main :: IO ()
main = r2AxisMain myaxis
