import Plots
import Plots.Types hiding (B)
import Plots.Types.Function
import Plots.Axis
import Diagrams.Prelude
import Diagrams.Coordinates.Polar

import Diagrams.Backend.Rasterific

myaxis :: Axis B Polar Double
myaxis = polarAxis &~ do
  addPlotable $
    mkParametricPlot (\t -> mkPolar t (t @@ rad))
      & parametricDomain . _2 .~ 8

make :: Diagram B -> IO ()
make = renderRasterific "examples/polar.png" (mkWidth 600)

main = make $ renderAxis myaxis

