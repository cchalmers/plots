{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ViewPatterns           #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Plots.Style
-- Copyright   :  (C) 2015 Christopher Chalmers
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Christopher Chalmers
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module defines the 'AxisStyle' type along with different colour
-- schemes. 'AxisStyle's are used to provide default colours and shapes
-- for the plots of an axis.
--
----------------------------------------------------------------------------

module Plots.Style
  ( -- * The axis style
    AxisStyle
  , HasAxisStyle (..)
  , axisStyles

    -- ** Predefined styles
  , fadedColours
  , vividColours
  , blackAndWhite

    -- * Plot Style
  , PlotStyle
  , HasPlotStyle (..)

  -- ** Applying Plot styles
  , applyLineStyle
  , applyMarkerStyle
  , applyAreaStyle
  , applyTextStyle

    -- * Colour schemes
  , colours1
  , colours2

    -- * Marker shapes
  , asterisk
  , diamond
  , crossShape
  , star'
  , plus
  , lineMarkers

    -- * Colour maps
  , ColourMap
  , ixColour
  , cmTraverse
  , colourMap
  , alphaColourMap
  , colourList
  , toStops

    -- ** Sample maps
  , hot

  ) where

-- import           Control.Lens     hiding (transform, ( # ), at, none)
import qualified Control.Lens     as Lens
import           Data.Colour.SRGB
import qualified Data.Map         as M
import           Data.Typeable
import           Diagrams.Prelude
import           Linear
import           Plots.Utils           (BackendType)

-- | A plot style is made up of separate styles ('lineStyle',
--   'markerStyle', 'areaStyle' and 'textStyle') a 'plotColour' and a
--   'plotMarker'. When rendering a plot, the 'PlotStyle's in an
--   'AxisStyle' are used to style each plot. The lenses can be used to
--   customise each style when adding the plot.
data PlotStyle b v n = PlotStyle
  { _plotColor   :: Colour Double
  , _lineStyle   :: Colour Double -> Style v n
  , _markerStyle :: Colour Double -> Style v n
  , _areaStyle   :: Colour Double -> Style v n
  , _textStyle   :: Colour Double -> Style v n
  , _plotMarker  :: QDiagram b v n Any
  } deriving Typeable
  -- XXX link to examples in haddock?

type instance V (PlotStyle b v n) = v
type instance N (PlotStyle b v n) = n
type instance BackendType (PlotStyle b v n) = b

makeLensesFor
  [ ("_plotColor",    "__plotColor")
  , ("_lineStyle",    "__lineStyleFunction")
  , ("_markerStyle",  "__markerStyleFunction")
  , ("_areaStyle",    "__areaStyleFunction")
  , ("_textStyle",    "__textStyleFunction")
  , ("_plotMarker",   "__plotMarker")
  -- For the traversal
  , ("_lineStyle",    "__plotStyleFunctions")
  , ("_markerStyle",  "__plotStyleFunctions")
  , ("_areaStyle",    "__plotStyleFunctions")
  , ("_textStyle",    "__plotStyleFunctions")
  ]
  ''PlotStyle

-- | Class for objects that contain a 'PlotStyle'.
class HasPlotStyle f a where
  -- | Lens onto the 'PlotStyle'.
  plotStyle :: LensLike' f a (PlotStyle (BackendType a) (V a) (N a))

  -- | The 'plotColor' is the overall colour of the plot. This is passed
  --   to the other styles ('lineStyle', 'markerStyle' etc.) to give an
  --   overall colour for the plot.
  plotColour :: Functor f => LensLike' f a (Colour Double)
  plotColour = plotStyle . __plotColor

  -- | Alias for 'plotColour'.
  plotColor :: Functor f => LensLike' f a (Colour Double)
  plotColor = plotStyle . __plotColor

  -- | This style is applied to any plots made up of lines only (like
  --   'Path' plots). This is a less general version of
  --   'lineStyleFunction'.
  lineStyle :: Settable f => LensLike' f a (Style (V a) (N a))
  lineStyle = lineStyleFunction . mapped

  -- | A version 'lineStyle' with access to the current 'plotColour'
  --   when 'applyLineStyle' is used.
  lineStyleFunction :: Functor f => LensLike' f a (Colour Double ->
    Style (V a) (N a))
  lineStyleFunction = plotStyle . __lineStyleFunction

  -- | This style is applied to any markers in the plot (usually the
  --   'plotMarker'). This is a less general version of
  --   'markerStyleFunction'.
  markerStyle :: Settable f => LensLike' f a (Style (V a) (N a))
  markerStyle = markerStyleFunction . mapped

  -- | A version 'lineStyle' with access to the current 'plotColour' when
  --   'applyMarkerStyle' is used.
  markerStyleFunction :: Functor f => LensLike' f a (Colour Double -> Style (V a) (N a))
  markerStyleFunction = plotStyle . __markerStyleFunction

  -- | This style is applied to any filled areas in a plot (like
  --   'Plots.Types.Bar' or 'Plots.Styles.Ribbon'). This is a less
  --   general version of 'areaStyleFunction'.
  areaStyle :: Settable f => LensLike' f a (Style (V a) (N a))
  areaStyle = areaStyleFunction . mapped

  -- | A version 'areaStyle' with access to the current 'plotColour' when
  --   'applyAreaStyle' is used.
  areaStyleFunction :: Functor f => LensLike' f a (Colour Double -> Style (V a) (N a))
  areaStyleFunction = plotStyle . __areaStyleFunction

  -- | This style is applied to text plots. This is a less general
  --   version of 'textStyleFunction'.
  textStyle :: Settable f => LensLike' f a (Style (V a) (N a))
  textStyle = textStyleFunction . mapped

  -- | A version 'textStyle' with access to the current 'plotColour' when
  --   'applyAreaStyle' is used.
  textStyleFunction :: Functor f => LensLike' f a (Colour Double -> Style (V a) (N a))
  textStyleFunction = plotStyle . __textStyleFunction

  -- | This diagram is used as any markers in a plot (like
  --   'Plots.Types.Scatter'). The 'markerStyle' will be applied to this
  --   marker when the plot gets rendered.
  plotMarker :: Functor f => LensLike' f a (QDiagram (BackendType a) (V a) (N a) Any)
  plotMarker = plotStyle . __plotMarker

  -- | A traversal over all the styles ('lineStyle', 'markerStyle',
  --  'areaStyle' and 'textStyle') of a 'PlotStyle'. This is a less
  --  general version of 'plotStyleFunctions'.
  plotStyles :: Settable f => LensLike' f a (Style (V a) (N a))
  plotStyles = plotStyleFunctions . mapped

  -- | A version of 'plotStyles' with access to the 'plotColour'.
  plotStyleFunctions :: Applicative f => LensLike' f a (Colour Double -> Style (V a) (N a))
  plotStyleFunctions = plotStyle . __plotStyleFunctions

instance HasPlotStyle f (PlotStyle b v n) where
  plotStyle = id

-- Applying styles -----------------------------------------------------

-- | Apply the 'lineStyle' from a 'PlotStyle'.
applyLineStyle
  :: (SameSpace a t, HasPlotStyle (Const (PlotStyle (BackendType a) (V a) (N a))) a, HasStyle t)
  => a -> t -> t
applyLineStyle (view plotStyle -> sty) =
  applyStyle $ (sty ^. lineStyleFunction) (sty ^. plotColour)

-- | Apply the 'markerStyle' from a 'PlotStyle'.
applyMarkerStyle
  :: (SameSpace a t, HasPlotStyle (Const (PlotStyle (BackendType a) (V a) (N a))) a, HasStyle t)
  => a -> t -> t
applyMarkerStyle (view plotStyle -> sty) =
  applyStyle $ (sty ^. markerStyleFunction) (sty ^. plotColour)

-- | Apply the 'areaStyle from a 'PlotStyle'.
applyAreaStyle
  :: (SameSpace a t, HasPlotStyle (Const (PlotStyle (BackendType a) (V a) (N a))) a, HasStyle t)
  => a -> t -> t
applyAreaStyle (view plotStyle -> sty) =
  applyStyle $ (sty ^. areaStyleFunction) (sty ^. plotColour)

-- | Apply the 'textStyle' from a 'PlotStyle'.
applyTextStyle
  :: (SameSpace a t, HasPlotStyle (Const (PlotStyle (BackendType a) (V a) (N a))) a, HasStyle t)
  => a -> t -> t
applyTextStyle (view plotStyle -> sty) =
  applyStyle $ (sty ^. textStyleFunction) (sty ^. plotColour)

instance (Metric v, Traversable v, OrderedField n) => Transformable (PlotStyle b v n) where
  transform t = (plotMarker %~ transform t) . (plotStyles %~ transform t)

------------------------------------------------------------------------
-- Predefined themes
------------------------------------------------------------------------

-- $predefined
-- There are only a few themes for now. Eventually there will be a wider
-- range of themes with better support for customisation.

-- | Theme using 'funColours' with faded fills and thick lines.
--
-- <<plots/faded-colours.svg#diagram=black-and-white&width=300>>
fadedColours :: (TypeableFloat n, Renderable (Path V2 n) b) => AxisStyle b V2 n
fadedColours = AxisStyle hot $
  zipWith mkStyle (cycle colours1) (cycle $ map stroke filledMarkers)
  where
    mkStyle c = PlotStyle c lineS fadeS fadeS fillS
    lineS c = mempty # lc c # lwO 3 --  normal
    fadeS c = mempty # fc (blend 0.1 white c) # lc c # lwO 1
    fillS c = mempty # fc c # lw none

-- | Theme using 'funColours' with no lines on 'areaStyle.
--
-- <<plots/vivid-colours.svg#diagram=black-and-white&width=300>>
vividColours :: (TypeableFloat n, Renderable (Path V2 n) b) => AxisStyle b V2 n
vividColours = AxisStyle hot $
  zipWith mkStyle (cycle colours2) (cycle $ map (scale 1.2 . stroke) filledMarkers)
  where
    mkStyle c = PlotStyle c lineS markS fillS fillS
    lineS c = mempty # lc c # lwO 3 -- normal
    markS c = mempty # fc c # lwO 1 # lc white
    fillS c = mempty # fc c # lw none

-- | Theme without any colours, useful for black and white documents.
--
-- <<plots/black-and-white.svg#diagram=black-and-white&width=300>>
blackAndWhite :: (TypeableFloat n, Renderable (Path V2 n) b) => AxisStyle b V2 n
blackAndWhite = AxisStyle greys $
  zipWith3 mkStyle (cycle colours) (cycle lineStyles) (cycle $ map stroke filledMarkers)
  where
    mkStyle c ls = PlotStyle c ls markerS (mk fc) (mk fc)
    mk f c = mempty # f c
    --
    markerS c  = mempty # fc white # lc c
    lineStyles = mempty : map (\ds -> mempty # dashingL ds 0) dashes
    dashes     = [[6,6], [6,2], [2,2], [6,8,3]]
    colours    = [black, blend 0.8 black white, blend 0.6 black white, blend 0.4 black white]

------------------------------------------------------------------------
-- Colours
------------------------------------------------------------------------

-- Unimaginative names :(

-- | A colourful colour set used for 'fadedColours'.
colours1 :: OrderedField n => [Colour n]
colours1 = cycle
  [ sRGB24 228 26  28
  , sRGB24 55  126 184
  , sRGB24 77  175 74
  , sRGB24 152 78  163
  , sRGB24 255 127 0
  , sRGB24 166 86  40
  , sRGB24 247 129 191
  , sRGB24 153 153 153
  ]

-- | Another colour set, used for 'vividColours'.
colours2 :: OrderedField n => [Colour n]
colours2 = cycle
  [ sRGB24 27  158 119
  , sRGB24 217 95  2
  , sRGB24 117 112 179
  , sRGB24 231 41  138
  , sRGB24 102 166 30
  , sRGB24 230 171 2
  , sRGB24 166 118 29
  , sRGB24 102 102 102
  ]

-- | Markers which have a filling, used for 'fadedColours' and
--   'vividColours'.
filledMarkers :: RealFloat n => [Path V2 n]
filledMarkers = map (centerXY . pathFromTrail) $ cycle
  [ circle 0.5
  , square 1
  , triangle 1
  , diamond (1 / sqrt 2)
  , pentagon 0.6
  , crossShape 1
  , plus 1
  , star' 0.8
  ]

-- | 'asterisk' markers with varying numbers of prongs.
lineMarkers :: OrderedField n => [Path V2 n]
lineMarkers = cycle
  [ asterisk 4 1 # rotateBy (1/8)
  , asterisk 6 1
  , asterisk 5 1
  , asterisk 2 1
  , asterisk 2 1 # rotateBy (1/4)
  , asterisk 10 1
  , asterisk 3 1
  , asterisk 3 1 # rotateBy (1/2)
  ]

------------------------------------------------------------------------
-- Shapes
------------------------------------------------------------------------

-- | Make an asterisk with @n@ spokes, each of length @l@.
asterisk :: OrderedField n => Int -> n -> Path V2 n
asterisk n x
  = mconcat . take n
  . iterate (rotateBy (1/fromIntegral n))
  $ (0 ^& 0) ~~ (0 ^& x)

-- | A rotated 'square'.
diamond :: (InSpace V2 n t, TrailLike t) => n -> t
diamond = trailLike . rotateBy (1/8) . square

-- | A rotated 'plus'.
crossShape :: (InSpace V2 n t, TrailLike t) => n -> t
crossShape = trailLike . rotateBy (1/8) . plus

-- | Filled in @+@ symbol.
plus :: (InSpace V2 n t, TrailLike t) => n -> t
plus x = trailLike . (`at` mkP2 (x/6) (x/6))
       . wrapTrail . glueLine . mconcat . take 4
       . iterate (rotateBy (1/4)) . onLineSegments init
       $ square (x/3)

-- | A filled in five sided start of size x.
star' :: (InSpace V2 n t, TrailLike t) => n -> t
star' x = trailLike . (`at` mkP2 (-x/6) (x/6))
        . wrapTrail . glueLine . mconcat . take 5
        . iterate (rotateBy (-1/5)) $ spoke
  where
    spoke = fromOffsets . map r2 $ [(x/6,x/2), (x/6,-x/2)]

-- showcase :: Renderable (Path V2 n) b => Theme -> Diagram b V2 n
-- showcase theme
--   = vcat' (with & sep .~ 0.4) . take 8
--   $ zipWith (\c m -> styleF c m <> lineBehind c) cs ms
--   where
--     lineBehind c = (p2 (-1,0) ~~ p2 (1,0)) # lc c # lwG 0.2

------------------------------------------------------------------------
-- Colour maps
------------------------------------------------------------------------

-- type ColourMap = [(Double, AlphaColour Double)]

-- | A map from a number (usually between 0 and 1) to a colour. Colour
--   maps are part of the 'AxisStyle', which is used for plots like
--   'Plots.Types.HeatMap'.
newtype ColourMap = ColourMap (M.Map Rational (AlphaColour Double))
  deriving Show


type instance V ColourMap = V1
type instance N ColourMap = Rational

-- makeWrapped ''ColourMap
-- manually write instance to avoid template haskell splitting the
-- module up
instance Rewrapped ColourMap ColourMap
instance Wrapped ColourMap where
  type Unwrapped ColourMap = M.Map Rational (AlphaColour Double)
  _Wrapped' = iso (\(ColourMap a) -> a) ColourMap
  {-# INLINE _Wrapped' #-}

p1apply :: Num a => Transformation V1 a -> a -> a
p1apply t a = papply t (P (V1 a)) ^. _x

instance Transformable ColourMap where
  transform t = over (_Wrapped' . _Wrapped' . mapped . _1) (p1apply t)

type instance Index ColourMap   = Rational
type instance IxValue ColourMap = AlphaColour Double

instance AsEmpty ColourMap where
  _Empty = nearly (ColourMap M.empty) (allOf each (==transparent))

instance Each ColourMap ColourMap (AlphaColour Double) (AlphaColour Double) where
  each = _Wrapped' . each

instance Ixed ColourMap where
  ix = ixColour

-- | 'Nothing' == 'transparent'
instance At ColourMap where
  at x = ixColour x . from (non transparent)

ixColour :: Rational -> Lens' ColourMap (AlphaColour Double)
ixColour x f (ColourMap cm) = f c <&> \c' -> ColourMap (M.insert x c' cm)
  where
  c = case (M.lookupLE x cm, M.lookupGE x cm) of
        (Just (i,c1), Just (j,c2))
          | i == j    -> c1
          | otherwise ->
              let a = fromRational $ (x - i) / (j - i)
              in  blend a c2 c1
        (Just (_,c1), Nothing) -> c1
        (Nothing, Just (_,c2)) -> c2
        _                      -> transparent

-- | Indexed traversal over the colours indexed and ordered by their
--   position in the map.
cmTraverse :: IndexedTraversal' Rational ColourMap (AlphaColour Double)
cmTraverse = _Wrapped' . itraversed

-- | Return the list of colours in the [0,1] range in order. This always
--   includes colours 0 and 1.
colourList :: ColourMap -> [(Rational, AlphaColour Double)]
colourList = itoListOf (cmTraverse . ifiltered (\i _ -> i >= 0 && i <= 1))
           . (ixColour 0 %~ id) . (ixColour 1 %~ id)
           -- touch colours at 0 and 1 so they're in the list

colourMap :: [(Rational, Colour Double)] -> ColourMap
colourMap = alphaColourMap . over (mapped . _2) opaque

alphaColourMap :: [(Rational, AlphaColour Double)] -> ColourMap
alphaColourMap [] = ColourMap M.empty
alphaColourMap cs
  | a == b        = ColourMap (M.singleton 0.5 c)
  | otherwise     = ColourMap (M.mapKeysMonotonic normalise cm)
  where
    cm    = M.fromList cs
    (a,c) = M.findMin cm
    (b,_) = M.findMax cm
    normalise x = (x - a) / (b - a)

toStops :: Fractional n => ColourMap -> [GradientStop n]
toStops = map (\(x,c) -> GradientStop (SomeColor c) (fromRational x))
        . colourList

hot :: ColourMap
hot = colourMap [(0, red), (1, yellow), (2, blue), (3, grey)]

greys :: ColourMap
greys = colourMap [(0, white), (1, black), (2, blue), (3, grey)]

------------------------------------------------------------------------
-- Axis Style
------------------------------------------------------------------------

-- | The 'AxisStyle' determines the 'Style's of the plots in an axis.
--   There are various predifined styles to change the look of the plot.
data AxisStyle b v n = AxisStyle
  { _axisColourMap :: ColourMap
  , _axisStyles    :: [PlotStyle b v n]
  }

type instance V (AxisStyle b v n) = v
type instance N (AxisStyle b v n) = n
type instance BackendType (AxisStyle b v n) = b

makeClassyFor
  "HasAxisStyle" "axisStyle"
  [ ("_axisColourMap", "axisColourMap")
  , ("_axisStyles",    "__axisStyles")
  ]
  ''AxisStyle

axisStyles :: HasAxisStyle a (BackendType a) (V a) (N a) =>
              IndexedTraversal' Int a (PlotStyle (BackendType a) (V a) (N a))
axisStyles = axisStyle . __axisStyles . traversed
