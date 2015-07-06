{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
import Plots
import Network.Wreq
import Control.Lens
import Data.Csv hiding ((.=))
import Plots.Axis
import qualified Data.Vector as V
import Data.ByteString.Lazy (ByteString)
import Control.Arrow
import Control.Monad.State (MonadState, execStateT)
import Data.Foldable
import Control.Monad.IO.Class
import Data.Time.Clock.POSIX
-- import Plots.Axis

import Diagrams.Backend.Rasterific
import Data.Time
import Control.Monad

import Diagrams
import Data.Maybe

-- Incomplete example using mtl to perform IO in the axis do notation.
-- The axis show dates but currently the tick positions and the start of
-- the dates are not aligned properly. (the ticks might be 1.2 years
-- apart but the labels will just show the year, which is misleading)

parseStocks :: ByteString -> [(String, Double)]
parseStocks bs = toListOf (each . to (view _1 &&& view _7)) v
  where
    Right v = decode HasHeader bs :: Either String (V.Vector (String, Double, Double, Double, Double, Double, Double))

filterStocks :: [(String, Double)] -> [(Double, Double)]
filterStocks = mapMaybe f
  where
    f (s, d) = do
      date  <- s      ^? timeFormat "%F"
      start <- "2014" ^? timeFormat "%Y"
      guard $ date > start
      return $ (date ^. realUTC, d)

myaxis :: IO (Axis B V2 Double)
myaxis = execStateT ?? r2Axis $ do
  goog <- liftIO $ get "http://ichart.yahoo.com/table.csv?s=GOOG"
  appl <- liftIO $ get "http://ichart.yahoo.com/table.csv?s=AAPL"
  for_ [goog, appl] $
    linePlotOf (responseBody . to (filterStocks . parseStocks) . each)
  axisTickLabels . _x . tickLabelFun .= autoTimeLabels

  xAxisLabel .= "date"
  yAxisLabel .= "closing (dollars)"

main :: IO ()
main = myaxis >>= make . renderAxis

make :: Diagram B -> IO ()
make = renderRasterific "examples/stocks.png" (mkWidth 600) . frame 30

------------------------------------------------------------------------

linePlotOf
  :: (PointLike V2 n p, TypeableFloat n, MonadState (Axis b V2 n) m, Renderable (Path V2 n) b)
  => Fold s p -- ^ Fold over data
  -> s        -- ^ Data
  -> m ()     -- ^ Monad action on axis
linePlotOf f s = addPlotable (Path [mkTrailOf f s])

------------------------------------------------------------------------
-- Time
------------------------------------------------------------------------

-- | Same as 'timeFormat' but with the option of choosing the
--   'TimeLocale'.
localeTimeFormat
  :: (ParseTime a, FormatTime a)
  => TimeLocale -> String -> Prism' String a
localeTimeFormat tl s = prism' (formatTime tl s) (parseTimeM False tl s)
{-# INLINE localeTimeFormat #-}

-- | A prism between a parse-able format and its string representation
--   from the given format string using the 'defaultTimeLocale'. See
--   'formatTime' for a description of the format string.
--
-- @
-- >>> timeFormat "%F" # ModifiedJulianDay 91424
-- "2109-03-10"
--
-- >>> "2109-03-10" ^? timeFormat "%F" :: Maybe UTCTime
-- Just 2109-03-10 00:00:00 UTC
-- @
--
timeFormat
  :: (ParseTime a, FormatTime a)
  => String -> Prism' String a
timeFormat = localeTimeFormat defaultTimeLocale
{-# INLINE timeFormat #-}

-- | Automatically choose a suitable time axis, based upon the time range
--   of data.

-- XXX: This is a terrible way to do it if the ticks aren't aligned
-- properly.
autoTimeLabels :: RealFloat n => [n] -> (n,n) -> [(n, String)]
autoTimeLabels ts (t0, t1)
  | d < minute = fmt "%S%Q"
  | d < hour   = fmt "%M:%S"
  | d < day    = fmt "%H:%M"
  | d < month  = fmt "%F %H"
  | d < year   = fmt "%F"
  | d < 2*year   = fmt "%F"
  | otherwise  = fmt "%Y"
  where
    d     = t1 - t0
    fmt a = map (\n -> (n, formatTime defaultTimeLocale a (realToUTC n))) ts

    minute = 60
    hour   = 60 * minute
    day    = 24 * hour
    month  = 30 * day
    year   = 365 * day

realToUTC :: Real a => a -> UTCTime
realToUTC = posixSecondsToUTCTime . realToFrac

realUTC :: (Real a, Fractional a) => Iso' UTCTime a
realUTC = iso (realToFrac . utcTimeToPOSIXSeconds) realToUTC

