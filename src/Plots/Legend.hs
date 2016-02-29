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

   -- * Legend configuration

   -- * Positioning
 , Position (..)
 , getPosition
 , Anchor (..)
 , anchor
 , alignTo

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

-- | The data type to describe how to draw a legend. For legend entries
--   see 'Plots.Axis.LegendEntry'.
data Legend b n = Legend
  { lPosition    :: Position
  , lAnchor      :: Anchor
  , lGap         :: V2 n
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
  legend :: Lens' a (Legend b (N a))

  -- | The 'Position' of the legend relative to the 'Plots.Axis.Axis'.
  legendPosition :: Lens' a Position
  legendPosition = legend . lens lPosition (\l a -> l {lPosition = a})

  -- | The anchor for where the legend is placed.
  legendAnchor :: Lens' a Anchor
  legendAnchor = legend . lens lAnchor (\l a -> l {lAnchor = a})

  -- | The gap between the legend and the axis.
  legendGap :: Lens' a (V2 (N a))
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

  -- | Whether the legend should be visible.
  legendVisible :: Lens' a Bool
  legendVisible = legend . lens lVisible (\l a -> l {lVisible = a})

instance HasLegend (Legend b n) b where
  legend = id

instance (TypeableFloat n, Renderable (Text n) b) => Default (Legend b n) where
  def = Legend
    { lPosition    = NorthEast
    , lAnchor      = AnchorTopLeft
    , lGap         = V2 20 0
    , lSpacing     = 20
    , lTextWidth   = 60
    , lStyle       = mempty
    , lTextF       = mkText (BoxAlignedText 0 0.5)
    , lTextStyle   = mempty & fontSize (output 8)
    , lOrientation = Vertical
    , lVisible     = True
    }

instance HasVisibility (Legend b n) where
  visible = legendVisible

instance TypeableFloat n => HasStyle (Legend b n) where
  applyStyle sty = over legendStyle (applyStyle sty)

instance HasOrientation (Legend b n) where
  orientation = legendOrientation

-- | Draw a legend to the bounding box using the legend entries and
--   legend options.
drawLegend
  :: (TypeableFloat n,
      Typeable b,
      Renderable (Path V2 n) b,
      Renderable (Text n) b)
  => BoundingBox V2 n
  -> [(QDiagram b V2 n Any, String)]
  -> Legend b n
  -> QDiagram b V2 n Any
drawLegend bb entries l
  | l ^. hidden || null entries = mempty
  | otherwise = alignTo (l ^. legendPosition)
                          bb
                          (l ^. legendAnchor)
                          (l ^. legendGap)
                          (ledge <> back)
  where
    w = l ^. legendTextWidth
    h = l ^. legendSpacing
    --
    ledge = map mkLabels entries
              # orient (l ^. legendOrientation) hcat vcat
              # alignTL

    back = rect w (h * fromIntegral (length entries))
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
