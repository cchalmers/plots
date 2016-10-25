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

import           Diagrams.BoundingBox
import           Diagrams.Prelude

import           Plots.Types

-- | The data type to describe how to draw a legend. For legend entries
--   see 'Plots.Types.LegendEntry'.
data Legend b n = Legend
  { lPlacement   :: Placement
  , lGap         :: n
  , lStyle       :: Style V2 n
  , lSpacing     :: n
  , lTextWidth   :: n
  , lTextF       :: String -> QDiagram b V2 n Any
  , lTextStyle   :: Style V2 n
  , lOrientation :: Orientation
  , lVisible     :: Bool
  } deriving Typeable

type instance V (Legend b n) = V2
type instance N (Legend b n) = n

class HasLegend a b | a -> b where
  -- | Lens onto the 'Legend' of something.
  legend :: Lens' a (Legend b (N a))

  -- | The 'Placement' of the legend relative to the 'Plots.Axis.Axis'.
  legendPlacement :: Lens' a Placement
  legendPlacement = legend . lens lPlacement (\l a -> l {lPlacement = a})

  -- | The gap between the legend and the axis.
  legendGap :: Lens' a (N a)
  legendGap = legend . lens lGap (\l a -> l {lGap = a})

  -- | The style applied to the surronding box of the legend.
  legendStyle :: Lens' a (Style V2 (N a))
  legendStyle = legend . lens lStyle (\l a -> l {lStyle = a})

  -- | The spacing between entries in the legend.
  legendSpacing :: Lens' a (N a)
  legendSpacing = legend . lens lSpacing (\l a -> l {lSpacing = a})

  -- | The space given for the text in the legend.
  legendTextWidth :: Lens' a (N a)
  legendTextWidth = legend . lens lTextWidth (\l a -> l {lTextWidth = a})

  -- | The function to generate the legend text.
  legendTextFunction :: Lens' a (String -> QDiagram b V2 (N a) Any)
  legendTextFunction = legend . lens lTextF (\l a -> l {lTextF = a})

  -- | The style applied to the legend text.
  legendTextStyle :: Lens' a (Style V2 (N a))
  legendTextStyle = legend . lens lTextStyle (\l a -> l {lTextStyle = a})

  -- | The way the legend entries are listed. (This will likely be
  --   replaced by a grid-like system)
  legendOrientation :: Lens' a Orientation
  legendOrientation = legend . lens lOrientation (\l a -> l {lOrientation = a})

instance HasLegend (Legend b n) b where
  legend = id

instance HasGap (Legend b n) where
  gap = legendGap

instance HasPlacement (Legend b n) where
  placement = legendPlacement

instance (TypeableFloat n, Renderable (Text n) b) => Default (Legend b n) where
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

instance HasVisibility (Legend b n) where
  visible = lens lVisible (\l a -> l {lVisible = a})

instance TypeableFloat n => HasStyle (Legend b n) where
  applyStyle sty = over legendStyle (applyStyle sty)

instance HasOrientation (Legend b n) where
  orientation = legendOrientation

-- | Draw a legend to the bounding box using the legend entries and
--   legend options.
drawLegend
  :: (TypeableFloat n,
      Renderable (Path V2 n) b)
  => BoundingBox V2 n                -- ^ bounding box to place legend against
  -> [(QDiagram b V2 n Any, String)] -- ^ diagram pictures along with their key
  -> Legend b n                      -- ^ options for drawing the legend
  -> QDiagram b V2 n Any             -- ^ rendered legend
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

    back = rect (w + h + 5) (h * fromIntegral (length entries))
             # fcA transparent
             # applyStyle (l ^. legendStyle)
             # alignTL
             # translate (V2 (-5) 0) -- (-3))

    -- mkLabels :: (QDiagram b V2 n Any, String) -> QDiagram b V2 n Any
    mkLabels (pic, txt) = pic' ||| strutX 5 ||| label where
      pic'  = pic # withEnvelope (fromCorners (pure (-h/2)) (pure (h/2)))
      label = view legendTextFunction l txt
                # applyStyle (l ^. legendTextStyle)
                # withEnvelope (fromCorners origin (mkP2 w h) # moveTo (mkP2 0 (-h/2)))

-- wrapPic :: RealFloat n => V2 n -> QDiagram b V2 n Any -> QDiagram b V2 n Any
-- wrapPic ((^/ 2) -> v) d
--   = d # sizedAs (fromCorners (origin .-^ v) (origin .+^ v))
