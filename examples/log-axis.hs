import Plots
import Diagrams.Backend.Rasterific.CmdLine
import Diagrams.Prelude

logData = [V2 1 10, V2 2 100, V2 2.5 316, V2 3 1000]

logAxis :: Axis B V2 Double
logAxis = r2Axis &~ do
  scatterPlot' logData
  -- yMin ?= 200

  yAxis &= do
    logScale .= LogAxis
    majorTicksFunction .= logMajorTicks 5 -- <> pure [1]
    -- minorTicksFunction .= minorTicksHelper 5

main = mainWith logAxis

