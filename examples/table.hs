{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
import Data.Data
import Data.Table
import Plots
import Diagrams.Prelude hiding (with)
import Diagrams.Backend.Rasterific
import Control.Arrow

data Foo = Foo
  { fooId :: Int
  , fooX  :: Double
  , fooY  :: Double }
  deriving (Eq,Ord,Show,Read,Data,Typeable)

makeClassy_ ''Foo

makeTabular 'fooId
  [ (''Supplemental, 'fooX)
  , (''Supplemental, 'fooY)
  ]

mytable :: Table Foo
mytable = [Foo 0 0.5 1.0, Foo 1 1.1 2.0, Foo 2 1.45 3.0, Foo 3 2.1 4.0, Foo 4 20.1 4.0, Foo 5 2.51 5.0] ^. table

to2 :: (s -> a) -> (s -> b) -> Getter s (a, b)
to2 f g = to (f &&& g)

myaxis :: Axis B V2 Double
myaxis = r2Axis &~ do
  scatterPlotOf (with fooX (<) 20 . each . to2 fooX fooY) mytable

make :: Diagram B -> IO ()
make = renderRasterific "examples/table.png" (mkWidth 600)

main :: IO ()
main = make $ renderAxis myaxis


