{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
-- | This example requires cassava for csv parsing.
-- module Criterion where
import Data.Csv hiding ((.=))
import Data.Typeable
import qualified Data.ByteString.Lazy as BS
import Diagrams.Prelude
import qualified Data.Vector as V
import System.IO.Unsafe
import Control.Applicative
import Diagrams.Backend.SVG
import Diagrams.Backend.SVG.CmdLine
import Plots
-- import Plots.Types.Bar
import Diagrams.TwoD.Text
import Control.Arrow ((&&&))
import Plots.Axis
import Diagrams.Backend.SVG
import Data.Monoid.Recommend
-- import Control.Arrow
import Data.Function
import Data.List (groupBy)
import Diagrams.Core.Transform
import Control.Monad.State
import Linear.V2
import Data.Bool
import qualified Debug.Trace as Debug
import Diagrams.Backend.CmdLine hiding (output)
import Plots.Types.Bar
import qualified Data.Foldable as F
import Data.Monoid (Endo (..))

-- Misc stuff ----------------------------------------------------------

barAxis :: Axis SVG V2 Double
barAxis = r2Axis &~ gridLineVisible .= False

-- Criterion csv parsing -----------------------------------------------

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

-- | Read a @.csv@ file from criterion's output.
readCriterion :: MonadIO m => FilePath -> m (V.Vector CResult)
readCriterion path = liftIO $ do
  csv <- BS.readFile path
  let Right v = decode HasHeader csv
  return v

-- | Group criterion results by name.
groupCriterion :: [CResult] -> [(String, [CResult])]
groupCriterion = map collate . groupBy ((==) `on` fst) . map splitName
  where
    splitName r = (a, r & name .~ tail b)
      where (a,b) = break (=='/') (r ^. name)
    collate []           = ("",[])
    collate as@((n,_):_) = (n, map snd as)

-- Making criterion plots ----------------------------------------------

  -- => BarPlotOpts n -> [a] -> (a -> [n]) -> (a -> State (PlotProperties b V2 n) ()) -> m ()

-- | Given a filepath to a criterion @.csv@ file, make an axis.
criterionAxis :: FilePath -> IO (Axis SVG V2 Double)
criterionAxis path = execStateT ?? barAxis $ do
  results <- readCriterion path

  let rss = groupCriterion (F.toList results)
  multiBars rss (map _mean . snd) $ do
    runningBars
    horizontal .= True
    labelBars (rss ^.. each . _2 . each . name)
    barWidth .= 0.6
    onBars $ \cresults -> key (fst cresults)

  modify hideMinorTicks
  xAxis . axisLabelText .= "average time (s)"
  xAxis . majorGridLineVisible .= True

-- instance HasOrientation p => HasOrientation (Plot p b) where
--   orientation = rawPlot . orientation

-- Groups bars ---------------------------------------------------------

-- groupedData :: [(String, [Double])]
-- groupedData =
--   [ ( "green"
--     , [ 7, 14, 3, 17 ]
--     )
--   , ( "blue"
--     , [ 12, 8, 12, 10 ]
--     )
--   , ( "orange"
--     , [ 20, 2, 19, 7 ]
--     )
--   ]

-- groupedAxis :: Axis SVG V2 Double
-- groupedAxis = barAxis &~ do
--   multiBars groupedData snd $ do
--     groupedBars' 0.4
--     labelBars ["fun", "professional", "bright", "cost"]
--     barWidth   *= 0.7
--     horizontal .= True

--     onBars $ \ (l,_) -> do
--       key l
--       areaStyle . mapped . _lw .= none
--       case readColourName l of
--         Just c  -> plotColor .= c
--         Nothing -> error l -- return ()

-- simpleBarAxis :: Axis SVG V2 Double
-- simpleBarAxis = barAxis &~ do
--   Plots.Types.Bar.barPlot [5,3,6,7,2] $ orientation .= Vertical

-- make :: Diagram SVG -> IO ()
-- make = renderSVG "examples/criterion4.png" (mkWidth 1000) . frame 30

main :: IO ()
main = mainWith criterionAxis
-- -- main = mainWith criterionAxis -- "examples/criterion.csv" >>= make . renderAxis
-- -- main = criterionAxis "examples/criterion.csv" >>= make . renderAxis

