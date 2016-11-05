import Plots
import Diagrams.Prelude
import Diagrams.Coordinates.Polar
import Diagrams.Coordinates.Isomorphic
import Diagrams.Backend.Rasterific.CmdLine
import Plots.Legend

ps :: [Polar Double]
ps = [ mkPolar x theta | x <- [35], theta <- [20@@deg, 40@@deg .. fullTurn] ]

myAxis :: Axis B Polar Double
myAxis = polarAxis &~ do
  scatterPlot ps $ key "points"

  let ps' = map (_r *~ 0.6) ps
  scatterPlot ps' $ key "points'"

  legendPlacement .= rightTop
  rLabel     .= "r-axis"
  thetaLabel .= "theta-axis"

main :: IO ()
main = mainWith myAxis

