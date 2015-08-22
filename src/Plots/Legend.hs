{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Plots.Legend where
 -- ( -- * Legend entries
 --   LegendEntry
 -- , legendText
 -- , legendPic
 -- , legendAnchor
 -- , legendGap
 --   -- * Legend configuration
 -- , legendPosition

 --   -- * Positioning
 -- , Position (..)
 -- , getPosition
 -- , Anchor (..)
 -- , anchor

 -- , legendOrientation
 -- , Legend
 -- , drawLegend
 -- ) where

import           Control.Lens         hiding (none, ( # ))
import           Data.Default
import           Data.Typeable
import           Diagrams.TwoD.Text

import           Diagrams.BoundingBox
import           Diagrams.Prelude hiding (view)

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
  | Position Rational Rational -- ^ @Position 0 0 = SouthWest@, @Position 1 1 = NorthWest@
  deriving (Show, Read, Eq, Ord)


data Anchor
  = AnchorTop
  | AnchorTopRight
  | AnchorRight
  | AnchorBottomRight
  | AnchorBottom
  | AnchorBottomLeft
  | AnchorLeft
  | AnchorTopLeft
  | Anchor Rational Rational -- ^ @Anchor 0 0 = AnchorBottomLeft@, @mkP2 1 1 = AnchorTopRight@
  deriving (Show, Read, Eq, Ord)


-- | Align an object using a given anchor.
anchor :: (InSpace V2 n a, Alignable a, HasOrigin a, Floating n)
       => Anchor -> a -> a
anchor a = case a of
  AnchorTop         -> alignT . centerX
  AnchorTopRight    -> alignTR
  AnchorRight       -> alignL . centerY
  AnchorBottomRight -> alignBR
  AnchorBottom      -> alignB . centerX
  AnchorBottomLeft  -> alignBL
  AnchorLeft        -> alignL . centerY
  AnchorTopLeft     -> alignTL
  Anchor x y        -> alignBy unitX (fromRational x) . alignBy unitY (fromRational y)

-- | Get the point from the 'Position' on the bounding box of the enveloped
--   object. Returns the origin if @a@ has an empty envelope.
getPosition :: (InSpace V2 n a, Enveloped a, HasOrigin a, Fractional n)
            => Position -> a -> P2 n
getPosition p a = flip (maybe origin) (getCorners $ boundingBox a)
  $ \(P (V2 xl yl), P (V2 xu yu)) ->
    P $ case p of
          North     -> V2 (mid xl xu) yu
          NorthEast -> V2 xu yu
          East      -> V2 xu (mid yl yu)
          SouthEast -> V2 xu yl
          South     -> V2 (mid xl xu) yl
          SouthWest -> V2 xl yl
          West      -> V2 xl (mid yl yu)
          NorthWest -> V2 xl yu
          Position x y -> V2 (lerp' x xu xl) (lerp' y xu xl)
  where mid l u = (l + u) / 2
        lerp' alpha u v = fromRational alpha * u + (1 - fromRational alpha) * v

-- XXX write more

-- | A tool for aligned one object to another.
alignTo :: (InSpace V2 n a, SameSpace a b, Enveloped a, HasOrigin a, Alignable b, HasOrigin b, Floating n)
  => Position -> a -> Anchor -> V2 n -> b -> b
alignTo p a an v b
  = b # anchor an
      # moveTo (getPosition p a .+^ v)

-- getPosition :: (Enveloped a, HasOrigin a, V a ~ V2, N a ~ n, Fractional n) => Position -> a -> P2 n
-- getPosition p a = case p of
--   North     -> lerp 0.5 tl tr
--   NorthEast -> tr
--   East      -> lerp 0.5 br tr
--   SouthEast -> br
--   South     -> lerp 0.5 br bl
--   SouthWest -> bl
--   West      -> lerp 0.5 bl tl
--   NorthWest -> tl
--   where
--     [bl, br, tl, tr] = getAllCorners $ boundingBox a

-- anchorAlign :: (Alignable a, Alignable b, V a ~ V b) => a -> Anchor -> Position -> V a ->

data Legend b n = Legend
  { _legendPosition    :: Position
  , _legendAnchor      :: Anchor
  , _legendGap         :: V2 n
  , _legendStyle       :: Style V2 n
  , _legendSpacing     :: n
  , _legendTextWidth   :: n
  , _legendTextF       :: String -> QDiagram b V2 n Any
  , _legendTextStyle   :: Style V2 n
  , _legendOrientation :: Orientation
  } deriving Typeable

type instance V (Legend b n) = V2
type instance N (Legend b n) = n

makeLenses ''Legend

instance (TypeableFloat n, Renderable (Text n) b) => Default (Legend b n) where
  def = Legend
          { _legendPosition    = NorthEast
          , _legendAnchor      = AnchorTopLeft
          , _legendGap         = V2 20 0
          , _legendSpacing     = 20
          , _legendTextWidth   = 60
          , _legendStyle       = mempty
          , _legendTextF       = mkText (BoxAlignedText 0 0.5)
          , _legendTextStyle   = mempty & fontSize (output 8)
          , _legendOrientation = Vertical
          }

instance TypeableFloat n => HasStyle (Legend b n) where
  applyStyle sty = over legendStyle (applyStyle sty)

drawLegend :: (TypeableFloat n, Typeable b, Renderable (Path V2 n) b, Renderable (Text n) b)
           => BoundingBox V2 n
           -> Legend b n
           -> [(Plot b V2 n, PlotProperties b V2 n)]
           -> QDiagram b V2 n Any
drawLegend bb l ps = alignTo (l ^. legendPosition)
                             bb
                             (l ^. legendAnchor)
                             (l ^. legendGap)
                             ledge
  where
    w = l ^. legendTextWidth
    h = l ^. legendSpacing
    --
    ledge      = orient (l ^. legendOrientation) hcat vcat
               $ concatMap mkLabels ps
    mkLabels (p,pp) = map mkLabel (pp ^. legendEntries)
      where
        mkLabel entry = pic ||| strutX 5 ||| txt
          where
            -- pps = p ^. plotProperties
            txt = (l ^. legendTextF) (entry ^. legendText)
                    # applyStyle (l ^. legendTextStyle)
                    # withEnvelope (fromCorners origin (mkP2 w h))
            pic = case entry ^. legendPic of
                    DefaultLegendPic  -> defLegendPic p pp
                    CustomLegendPic f -> f $ pp ^. plotStyle

wrapPic :: RealFloat n => V2 n -> QDiagram b V2 n Any -> QDiagram b V2 n Any
wrapPic ((^/ 2) -> v) d
  = d # sizedAs (fromCorners (origin .-^ v) (origin .+^ v))
