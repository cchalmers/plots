{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Plots.Legend
 ( -- * Legend entries
   LegendEntry
 , legendText
 , legendPic
 , legendAnchor
 , legendGap
   -- * Legend configuration
 , legendPosition

   -- * Positioning
 , Position (..)
 , Anchor (..)
 , getPosition

 , legendOrientation
 , Legend
 , drawLegend
 ) where

import           Control.Lens         hiding (none, ( # ))
import           Data.Default
import           Data.Typeable
import           Diagrams.TwoD.Text

import           Diagrams.BoundingBox
import           Diagrams.Prelude

import           Plots.Themes
import           Plots.Types

data Position
  = North
  | NorthEast
  | East
  | SouthEast
  | South
  | SouthWest
  | West
  | NorthWest

data Anchor
  = AnchorTop
  | AnchorTopRight
  | AnchorRight
  | AnchorBottomRight
  | AnchorBottom
  | AnchorBottomLeft
  | AnchorLeft
  | AnchorTopLeft

-- anchor :: (Alignable a, HasOrigin a, V a ~ R2) => Position -> a -> a
-- anchor a = case a of
--   Top         -> alignT . centerX
--   TopRight    -> alignTR
--   Right       -> alignL . centerY
--   BottomRight -> alignBR
--   Bottom      -> alignB . centerX
--   BottomLeft  -> alignBL
--   Left        -> alignL . centerY
--   TopLeft     -> alignTL

-- | Get the point from the 'Position' on the bounding box of the enveloped object.
getPosition :: (Enveloped a, HasOrigin a, V a ~ V2, N a ~ n, Fractional n) => Position -> a -> P2 n
getPosition p a = case p of
  North     -> (tl ^+^ tr) ^/ 2
  -- North     -> lerp 0.5 tl tr
  NorthEast -> tr
  East      -> (br ^+^ tr) ^/ 2
  -- East      -> lerp 0.5 br tr
  SouthEast -> br
  South     -> (br ^+^ bl) ^/ 2
  -- South     -> lerp 0.5 br bl
  SouthWest -> bl
  West      -> (bl ^+^ tl) ^/ 2
  -- West      -> lerp 0.5 bl tl
  NorthWest -> tl
  where
    [bl, br, tl, tr] = getAllCorners $ boundingBox a

-- anchorAlign :: (Alignable a, Alignable b, V a ~ V b) => a -> Anchor -> Position -> V a ->

data Legend b n = Legend
  { _legendPosition    :: Position
  , _legendAnchor      :: Anchor
  , _legendGap         :: V2 n
  , _legendStyle       :: Style V2 n
  , _legendTextF       :: String -> Diagram b V2 n
  , _legendTextStyle   :: Style V2 n
  , _legendOrientation :: Orientation
  } deriving Typeable

type instance V (Legend b n) = V2
type instance N (Legend b n) = n

makeLenses ''Legend

instance (DataFloat n, Renderable (Text n) b) => Default (Legend b n) where
  def = Legend
          { _legendPosition    = NorthEast
          , _legendAnchor      = AnchorTopLeft
          , _legendGap         = mkR2 20 20
          , _legendStyle       = mempty
          , _legendTextF       = text
          , _legendTextStyle   = mempty # fontSizeG 12
          , _legendOrientation = Verticle
          }

instance TypeableFloat n => HasStyle (Legend b n) where
  applyStyle sty = over legendStyle (applyStyle sty)

drawLegend :: (DataFloat n, Typeable v, Typeable b, Renderable (Path V2 n) b, Renderable (Text n) b)
           => Legend b n -> [Plot b v n] -> Diagram b V2 n
drawLegend l ps = orient (l ^. legendOrientation) hcat vcat
                $ concatMap mkLabels ps
  where
    mkLabels p = map mkLabel (p ^. legendEntries)
      where
        mkLabel entry = txt ||| pic
          where
            txt = (l ^. legendTextF) (entry ^. legendText)
                    # applyStyle (l ^. legendTextStyle)
               <> rect 60 10 # lw none -- find a better way to do this
            pic = case entry ^. legendPic of
                    DefaultLegendPic  -> defLegendPic p
                    CustomLegendPic f -> f $ p ^. themeEntry


