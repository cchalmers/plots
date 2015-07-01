{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
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

data CResult = CResult
  { _name     :: !String
  , _mean     :: !Double
  , _meanLB   :: !Double
  , _meanUB   :: !Double
  , _stddev   :: !Double
  , _stddevLB :: !Double
  , _stddevUB :: !Double
  }

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

make :: Diagram B -> IO ()
make = renderRasterific "examples/criterion.png" (mkWidth 600) . frame 30

main :: IO ()
main = make $ renderAxis myaxis

------------------------------------------------------------------------

namedBarPlotOf
  :: (Typeable b,
      Renderable (Text n) b,
      Renderable (Path V2 n) b,
      TypeableFloat n)
  => IndexedFold String s n -> s -> AxisState b V2 n
namedBarPlotOf l s = do
  let (nms, xs) = unzip $ itoListOf l s
  addPlotable $ simpleBarPlot xs
  axisTickLabels . _x . tickLabelFunction .= \_ _ a -> imap (rotatedLabel a) nms
  axisTickLabels . _y . tickLabelFunction .=
    atMajorTicks (\txtA n -> mkText txtA (show (toD n)))
  axisTicks . _x . majorTicksFun . mapped .= map fromIntegral [1 .. length xs]
  yMin .= Commit 0

invertAlign :: TextAlignment n -> TextAlignment n
invertAlign (BoxAlignedText x y) = BoxAlignedText y x
invertAlign a = a

rotatedLabel
  :: (Renderable (Text n) b,
      TypeableFloat n)
  => TextAlignment n -> Int -> String -> (n, QDiagram b V2 n Any)
rotatedLabel a x nm =
  (fromIntegral x + 1, mkText (invertAlign a) nm # rotateBy (1/12))

toD :: Real a => a -> Float
toD = realToFrac

barAxis :: Axis B V2 Double
barAxis = r2Axis &~ do
  -- axisTickLabels . _x . tickLabelFunction .= stringLabels mkText nms
  noGridLines
  noMinorTicks


