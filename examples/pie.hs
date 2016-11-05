import Diagrams.Prelude
import Plots
import Diagrams.Coordinates.Polar
import Diagrams.Backend.Rasterific.CmdLine
import Control.Lens.Operators ((&~))
import Control.Monad (when)

pieData :: [(String, Double)]
pieData = [("red", 3), ("blue", 6), ("green", 9), ("purple", 4)]

pieAxis :: Axis Rasterific Polar Double
pieAxis = polarAxis &~ do
  piePlot pieData snd $ do
    wedgeKeys fst
      -- when (nm=="red") $ wedgeOffset .= 0.2
      -- wedgeWidth %= (^/ 2)
  -- wedgeInnerRadius .= 0.5

  -- scatterPlot' [zero, V2 1 1, V2 (-2) 0.5]

  -- connectingLine .= True

main :: IO ()
-- main = r2AxisMain pieAxis -- renderPGF "examples_output/pie.pdf" (mkWidth 500) (renderAxis pieAxis)
main = mainWith pieAxis
-- main = renderRasterific "test.png" absolute (renderAxis pieAxis)

