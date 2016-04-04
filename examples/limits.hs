{-# LANGUAGE FlexibleContexts #-}

module Limits where

import Plots
-- import Plots.Axis
-- import Plots.Themes
-- import Plots.Types
import Diagrams.Prelude
import Diagrams.Backend.Rasterific
-- import Data.Monoid.Recommend

mydata1 = [(1,3), (2,5.5), (3.2, 6), (3.5, 6.1)]
mydata2 = mydata1 & each . _1 *~ 0.5
mydata3 = [V2 1.2 2.7, V2 2 5.1, V2 3.2 2.6, V2 3.5 5]

myaxis :: Axis B V2 Double
myaxis = r2Axis &~ do
  scatterPlot mydata1 $ key "data 1"
  scatterPlot mydata2 $ key "data 2"
  scatterPlot mydata3 $ key "data 3"

  -- The axis minimum and maximum are :: Maybe n. Where 'Nothing' uses
  -- the infered bounds from the axis data and 'Just a' uses a as the
  -- bound. To set the bound you can use the ?= operator or
  -- equivilantly, .= Just a
  xMin ?= 0
  xMax .= Just 10

  -- Coordinate labels are stored in the 'Axis' under axisLabels.
  -- Changing the label text is easy:
  xAxisLabel .= "x-axis"
  yAxisLabel .= "y-axis"

  -- More advanced things like changing text rendering or position of
  -- axis label can be changed by lenses onto the 'AxisLabel' for that
  -- axis.
  axisLabelStyle %= fc red
  xAxis . axisLabelPosition .= UpperAxisLabel
  yAxis . axisLabelStyle . _fontSize .= local 12

make :: Diagram B -> IO ()
make = renderRasterific "examples/limits.png" (mkWidth 600) . frame 20

main :: IO ()
main = make $ renderAxis myaxis


