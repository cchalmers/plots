{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Criterion where
import Data.Csv hiding ((.=))
import Data.Typeable
import qualified Data.ByteString.Lazy as BS
import Diagrams.Prelude
import qualified Data.Vector as V
import System.IO.Unsafe
import Control.Applicative
-- import Diagrams.Backend.PGF
import Plots
import Plots.Types.Bar
import Diagrams.TwoD.Text
import Plots.Axis
import Diagrams.Backend.Rasterific
import Data.Monoid.Recommend
import Control.Arrow
import Data.Function
import Data.List (groupBy)

data CResult = CResult
  { _name     :: !String
  , _mean     :: !Double
  , _meanLB   :: !Double
  , _meanUB   :: !Double
  , _stddev   :: !Double
  , _stddevLB :: !Double
  , _stddevUB :: !Double
  } deriving Show

makeLenses ''CResult

instance FromNamedRecord CResult where
  parseNamedRecord m =
    CResult <$> m .: "Name"     <*> m .: "Mean"     <*>
                m .: "MeanLB"   <*> m .: "MeanUB"   <*>
                m .: "Stddev"   <*> m .: "StddevUB" <*>
                m .: "StddevLB"

instance FromRecord CResult where
  parseRecord v
    | V.length v == 7 =
        CResult <$> v .! 0 <*> v .! 1 <*> v .! 2 <*>
                    v .! 3 <*> v .! 4 <*> v .! 5 <*>
                    v .! 6
    | otherwise     = empty

results :: V.Vector CResult
results = unsafePerformIO $ do
  csv <- BS.readFile "examples/criterion.csv"
  let Right v = decode HasHeader csv
  return v

myaxis :: Axis B V2 Double
myaxis = barAxis &~ do
  namedBarPlotOf (each . ito (_name &&& _mean)) results

mymultiaxis :: Axis B V2 Double
mymultiaxis = barAxis &~ do
  multiBarPlot (over (each . _2 . each) (_name &&& _mean) . groupage $ toListOf each results)

make :: Diagram B -> IO ()
make = renderRasterific "examples/criterion.png" (mkWidth 600) . frame 30

main :: IO ()
main = make $ renderAxis mymultiaxis

groupage :: [CResult] -> [(String, [CResult])]
groupage = map collate . groupBy ((==) `on` fst) . map splitName
  where
    splitName r = (a, r & name .~ tail b)
      where (a,b) = break (=='/') (r ^. name)
    collate []           = ("",[])
    collate as@((n,_):_) = (n, map snd as)

------------------------------------------------------------------------

multiBarPlot :: [(String, [(String, Double)])] -> AxisState B V2 Double
multiBarPlot d = do
  let (nms, xs) = unzip d
  yMin .= Commit 0
  -- axisTickLabels . _y . tickLabelFunction .=
  --   atMajorTicks (\txtA n -> mkText txtA (show (toD n)))
  axisTickLabels . _x . tickLabelFun .= stringLabels nms
  addPlotable $ multiBar (map (map snd) xs)

multiBar :: [[Double]] -> BarPlot Double
multiBar dds = BarPlot
  { _barData      = zip [1..] dds
  , _barWidth     = 0.5
  , _barSpacing   = 0.1
  , _verticleBars = False
  , _stacked      = False    -- whether the bars stacked (or side by side)
  }


-- ("pcg-fast",[("Word32",2.5478246508587825e-9),("Word32B",7.617835087754578e-9)])
-- ("pcg-single",[("Word32",2.489457565765152e-9)])
-- ("pcg-unique",[("Word32",2.5287319035400075e-9)])
-- ("mwc",[("Word64",9.031117576890663e-9),("Word32R",1.5986489045645512e-8),("Double",8.92557813101465e-9)])
-- ("mersenne",[("Word64",5.518793701316405e-9),("Double",7.210547645393152e-9)])

namedBarPlotOf
  :: (Typeable b,
      Renderable (Text n) b,
      Renderable (Path V2 n) b,
      TypeableFloat n)
  => IndexedFold String s n -> s -> AxisState b V2 n
namedBarPlotOf l s = do
  let (nms, xs) = unzip $ itoListOf l s
  addPlotable $ simpleBarPlot xs
  axisTickLabels . _x . tickLabelFun .= stringLabels nms
  axisTickLabels . _x . tickLabelTextFun .= rotatedLabel
  axisTicks . _x . majorTicksFun . mapped .= map fromIntegral [1 .. length xs]
  yMin .= Commit 0

invertAlign :: TextAlignment n -> TextAlignment n
invertAlign (BoxAlignedText x y) = BoxAlignedText y x
invertAlign a = a

rotatedLabel :: (Renderable (Text n) b, TypeableFloat n)
  => TextAlignment n -> String -> QDiagram b V2 n Any
rotatedLabel a l = (mkText (invertAlign a) l # rotateBy (1/12))

toD :: Real a => a -> Float
toD = realToFrac

barAxis :: Axis B V2 Double
barAxis = r2Axis &~ do
  -- axisTickLabels . _x . tickLabelFunction .= stringLabels mkText nms
  noGridLines
  noMinorTicks


