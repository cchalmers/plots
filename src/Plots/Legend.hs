{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
module Plots.Legend
 (
   -- * Legend
   Legend
 , HasLegend (..)

   -- * Drawing a legend
 , drawLegend

 ) where

import           Control.Lens         hiding (none, ( # ))
import           Data.Default
import           Data.Typeable
import           Diagrams.TwoD.Text

import           Geometry.BoundingBox
import           Diagrams.Prelude     hiding (orient)

import           Plots.Types

-- | The data type to describe how to draw a legend. For legend entries
--   see 'Plots.Types.LegendEntry'.
data Legend = Legend
  { lPlacement   :: Placement
  , lGap         :: Double
  , lStyle       :: Style V2 Double
  , lSpacing     :: Double
  , lTextWidth   :: Double
  , lTextF       :: String -> Diagram V2
  , lTextStyle   :: Style V2 Double
  , lOrientation :: Orientation
  , lVisible     :: Bool
  } deriving Typeable

type instance V Legend = V2
type instance N Legend = Double

class HasLegend a where
  -- | Lens onto the 'Legend' of something.
  legend :: Lens' a Legend

  -- | The 'Placement' of the legend relative to the 'Plots.Axis.Axis'.
  legendPlacement :: Lens' a Placement
  legendPlacement = legend . lens lPlacement (\l a -> l {lPlacement = a})

  -- | The gap between the legend and the axis.
  legendGap :: Lens' a Double
  legendGap = legend . lens lGap (\l a -> l {lGap = a})

  -- | The style applied to the surronding box of the legend.
  legendStyle :: Lens' a (Style V2 Double)
  legendStyle = legend . lens lStyle (\l a -> l {lStyle = a})

  -- | The spacing between entries in the legend.
  legendSpacing :: Lens' a Double
  legendSpacing = legend . lens lSpacing (\l a -> l {lSpacing = a})

  -- | The space given for the text in the legend.
  legendTextWidth :: Lens' a Double
  legendTextWidth = legend . lens lTextWidth (\l a -> l {lTextWidth = a})

  -- | The function to generate the legend text.
  legendTextFunction :: Lens' a (String -> Diagram V2)
  legendTextFunction = legend . lens lTextF (\l a -> l {lTextF = a})

  -- | The style applied to the legend text.
  legendTextStyle :: Lens' a (Style V2 Double)
  legendTextStyle = legend . lens lTextStyle (\l a -> l {lTextStyle = a})

  -- | The way the legend entries are listed. (This will likely be
  --   replaced by a grid-like system)
  legendOrientation :: Lens' a Orientation
  legendOrientation = legend . lens lOrientation (\l a -> l {lOrientation = a})

instance HasLegend Legend where
  legend = id

instance HasGap Legend where
  gap = legendGap

instance HasPlacement Legend where
  placement = legendPlacement

instance Default Legend where
  def = Legend
    { lPlacement   = rightTop
    , lGap         = 20
    , lSpacing     = 20
    , lTextWidth   = 60
    , lStyle       = mempty
    , lTextF       = mkText (BoxAlignedText 0 0.5)
    , lTextStyle   = mempty & fontSize (output 11)
    , lOrientation = Vertical
    , lVisible     = True
    }

instance HasVisibility Legend where
  visible = lens lVisible (\l a -> l {lVisible = a})

instance ApplyStyle Legend

-- | The style for the bounding box of the legend.
instance HasStyle Legend where
  style = legendStyle
  {-# INLINE style #-}

instance HasOrientation Legend where
  orientation = legendOrientation

-- | Draw a legend to the bounding box using the legend entries and
--   legend options.
drawLegend
  :: BoundingBox V2 Double  -- ^ bounding box to place legend against
  -> [(Diagram V2, String)] -- ^ diagram pictures along with their key
  -> Legend                 -- ^ options for drawing the legend
  -> Diagram V2             -- ^ rendered legend
drawLegend bb entries l
  | l ^. hidden || null entries = mempty
  | otherwise   = placeAgainst
                    bb
                    (l ^. legendPlacement)
                    (l ^. legendGap)
                    (ledge <> back)
  where
    w = l ^. legendTextWidth
    h = l ^. legendSpacing
    --
    ledge = map mkLabels entries
              # orient (l ^. legendOrientation) hcat vcat
              # alignTL

    back = backRect
             # applyStyle (l ^. legendStyle)
             # alignTL
    backRect = orient (l ^. legendOrientation)
      (rect (nEntries * entryWidth) h             )
      (rect entryWidth              (h * nEntries))
    nEntries = fromIntegral (length entries)

    -- Each legend picture has a width equal to the height of each
    -- legend entry. The picture also has a 5 unit buffer either side of
    -- it.
    entryWidth = w + 10 + h

    mkLabels (pic, txt) = strutX 5 ||| pic' ||| strutX 5 ||| label where
      pic'  = pic # withEnvelope (fromCorners (pure (-h/2)) (pure (h/2)))
      label = view legendTextFunction l txt
                # applyStyle (l ^. legendTextStyle)
                # withEnvelope (fromCorners origin (mkP2 w h) # moveTo (mkP2 0 (-h/2)))

-- wrapPic :: RealFloat n => V2 n -> QDiagram b V2 n Any -> QDiagram b V2 n Any
-- wrapPic ((^/ 2) -> v) d
--   = d # sizedAs (fromCorners (origin .-^ v) (origin .+^ v))
