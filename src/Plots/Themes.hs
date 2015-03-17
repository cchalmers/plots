{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes  #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module Plots.Themes
  ( -- * Theme
    Theme
  , themeColors
  , themeMarkers

    -- * Plot Style
  , PlotStyle
  , HasPlotStyle (..)
  , applyLineStyle
  , applyMarkerStyle
  , applyBarStyle

    -- * Common themes
  , coolTheme
  , corperateTheme

    -- * Colour schemes
  , colourfullColours
  -- , corperateTheme

    -- * Marker shapes
  , ThemeContructor
  , constructorColours
  , constructLineStyle
  , constructMarkerStyle
  , constructTheme

    -- * Marker shapes
  , prong
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

import           Control.Lens     hiding (transform, ( # ))
import           Data.Colour.SRGB
import           Data.Typeable
import           Diagrams.Prelude
import qualified Data.Map as M
import Linear

-- | A plot style is made up of separate styles for the line, marker and
--   fill aspects of a plot. It also contains a marker in the form of a
--   'Diagram' and a maybe colour which is applied to everything at the
--   end.
data PlotStyle b v n = PlotStyle
  { _plotColor   :: Colour Double
  , _lineStyle   :: Colour Double -> Style v n
  , _markerStyle :: Colour Double -> Style v n
  , _barStyle    :: Colour Double -> Style v n
  , _plotMarker  :: QDiagram b v n Any
  } deriving Typeable

instance (Metric v, Typeable n, OrderedField n) => Semigroup (PlotStyle b v n) where
  PlotStyle c ls1 ms1 bs1 m1 <> PlotStyle _ ls2 ms2 bs2 m2
    = PlotStyle c (ls1 <> ls2) (ms1 <> ms2) (bs1 <> bs2) (m1 <> m2)

instance (Metric v, Typeable n, OrderedField n) => Monoid (PlotStyle b v n) where
  mappend = (<>)
  mempty  = PlotStyle black mempty mempty mempty mempty

type instance V (PlotStyle b v n) = v
type instance N (PlotStyle b v n) = n

class HasPlotStyle a b | a -> b where
  plotStyle :: Lens' a (PlotStyle b (V a) (N a))
  {-# MINIMAL plotStyle #-}

  plotColor :: Lens' a (Colour Double)
  plotColor = plotStyle . lens _plotColor (\p f -> p {_plotColor = f})

  lineStyle :: Lens' a (Colour Double -> Style (V a) (N a))
  lineStyle = plotStyle . lens _lineStyle (\p f -> p {_lineStyle = f})

  markerStyle :: Lens' a (Colour Double -> Style (V a) (N a))
  markerStyle = plotStyle . lens _markerStyle (\p f -> p {_markerStyle = f})

  barStyle :: Lens' a (Colour Double -> Style (V a) (N a))
  barStyle = plotStyle . lens _barStyle (\p f -> p {_barStyle = f})

  plotMarker :: Lens' a (QDiagram b (V a) (N a) Any)
  plotMarker = plotStyle . lens _plotMarker (\p f -> p {_plotMarker = f})

instance HasPlotStyle (PlotStyle b v n) b where
  plotStyle = id

-- | Apply the line style from a plot style.
applyLineStyle :: (SameSpace a t, HasPlotStyle a b, HasStyle t) => a -> t -> t
applyLineStyle a = applyStyle $ (a ^. lineStyle) (a ^. plotColor)

-- | Apply the marker style from a plot style.
applyMarkerStyle :: (SameSpace a t, HasPlotStyle a b, HasStyle t) => a -> t -> t
applyMarkerStyle a = applyStyle $ (a ^. markerStyle) (a ^. plotColor)

-- | Apply the fill style from a plot style.
applyBarStyle :: (SameSpace a t, HasPlotStyle a b, HasStyle t) => a -> t -> t
applyBarStyle a = applyStyle $ (a ^. barStyle) (a ^. plotColor)

plotStyles :: HasPlotStyle a b => Traversal' a (Colour Double -> Style (V a) (N a))
plotStyles = plotStyle . t
  where
    t f PlotStyle {..} = PlotStyle
      <$> pure _plotColor
      <*> f _lineStyle
      <*> f _markerStyle
      <*> f _barStyle
      <*> pure _plotMarker

instance (Metric v, Traversable v, OrderedField n) => Transformable (PlotStyle b v n) where
  transform t = (plotMarker %~ transform t) . (plotStyles . mapped %~ transform t)

-- * A theme can be applied to multiple plots in an axis.
type Theme b v n = [PlotStyle b v n]

-- | Traversal over the colours of a theme.
themeColors :: Traversal' (Theme b v n) (Colour Double)
themeColors = each . plotColor

-- | Traversal over the markers of a theme.
themeMarkers :: Traversal' (Theme b v n) (Colour Double)
themeMarkers = each . plotColor

-- * Theme construction

-- | Convenient way to construct themes.
data ThemeContructor v n = ThemeContructor
  { _constructorColours   :: [Colour Double]
  , _constructLineStyle   :: Colour Double -> Style v n
  , _constructMarkerStyle :: Colour Double -> Style v n
  , _constructFillStyle   :: Colour Double -> Style v n
  , _markerPaths          :: [Path v n]
  }

makeLenses ''ThemeContructor

------------------------------------------------------------------------
-- Theme construction
------------------------------------------------------------------------

-- constructTheme :: (TypeableFloat n, Renderable (Path V2 n) b) => ThemeContructor n -> Theme b n
-- constructTheme tc = zipWith5 PlotStyle
--   colours
--   (tc^.constructLineStyle   <$> colours)
--   (tc^.constructMarkerStyle <$> colours)
--   (tc^.constructFillStyle   <$> colours)
--   (stroke                   <$> tc^.markerPaths)
--   where colours = tc ^. constructorColours

constructTheme :: (TypeableFloat n, Renderable (Path V2 n) b) => ThemeContructor V2 n -> Theme b V2 n
constructTheme tc = zipWith
  (\c s -> PlotStyle c
                     (tc ^. constructLineStyle)
                     (tc ^. constructMarkerStyle)
                     (tc ^. constructFillStyle)
                     s) (tc ^. constructorColours) (map stroke $ tc ^. markerPaths)

coolThemeConstructor :: TypeableFloat n => ThemeContructor V2 n
coolThemeConstructor = ThemeContructor
  { _constructorColours   = corperateColours
  , _constructLineStyle   = \c -> mempty
                                    # lc c
  , _constructMarkerStyle = \c -> mempty
                                    # lc c
                                    # fc (blend 0.9 c white)
  , _constructFillStyle   = \c -> mempty
                                    # lc c
                                    # fc (blend 0.9 c white)
  , _markerPaths          = filledMarkers # scale 5
  }


coolTheme :: (TypeableFloat n, Renderable (Path V2 n) b) => Theme b V2 n
coolTheme = constructTheme coolThemeConstructor

corperateTheme :: (TypeableFloat n, Renderable (Path V2 n) b) => Theme b V2 n
corperateTheme = constructTheme $
  coolThemeConstructor
    & constructorColours .~ corperateColours

------------------------------------------------------------------------
-- Colours
------------------------------------------------------------------------


colourfullColours :: OrderedField n => [Colour n]
colourfullColours = cycle
  [ sRGB24 228 26  28
  , sRGB24 55  126 184
  , sRGB24 77  175 74
  , sRGB24 152 78  163
  , sRGB24 255 127 0
  , sRGB24 166 86  40
  , sRGB24 247 129 191
  , sRGB24 153 153 153
  ]

corperateColours :: OrderedField n => [Colour n]
corperateColours = cycle
  [ sRGB24 27  158 119
  , sRGB24 217 95  2
  , sRGB24 117 112 179
  , sRGB24 231 41  138
  , sRGB24 102 166 30
  , sRGB24 230 171 2
  , sRGB24 166 118 29
  , sRGB24 102 102 102
  ]

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

lineMarkers :: OrderedField n => [Path V2 n]
lineMarkers = cycle
  [ prong 4 1 # rotateBy (1/8)
  , prong 6 1
  , prong 5 1
  , prong 2 1
  , prong 2 1 # rotateBy (1/4)
  , prong 10 1
  , prong 3 1
  , prong 3 1 # rotateBy (1/2)
  ]

------------------------------------------------------------------------
-- Shapes
------------------------------------------------------------------------

prong :: OrderedField n => Int -> n -> Path V2 n
prong n x = mconcat . take n
          . iterate (rotateBy (1/fromIntegral n))
          $ spoke
  where
    spoke = (0 ^& 0) ~~ (0 ^& x)

diamond :: (TrailLike t, Transformable t, V t ~ V2, N t ~ n, RealFloat n)
        => n -> t
diamond = rotateBy (1/8) . square

crossShape :: RealFloat n => n -> Trail V2 n
crossShape = rotateBy (1/8) . plus

plus :: RealFloat n => n -> Trail V2 n
plus x = wrapTrail . glueLine . mconcat . take 4
       . iterate (rotateBy (1/4)) . onLineSegments init
       $ square (x/3)


star' :: OrderedField n => n -> Trail V2 n
star' x = wrapTrail . glueLine . mconcat . take 5
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

newtype ColourMap = ColourMap (M.Map Rational (AlphaColour Double))
  deriving Show

makeWrapped ''ColourMap

type instance V ColourMap = V1
type instance N ColourMap = Rational

-- instance t ~ V1 b => Rewrapped (V1 a) t
-- instance Wrapped (V1 a) where
--   type Unwrapped (V1 a) = a
--   _Wrapped' = iso (\(V1 a) -> a) V1
--   {-# INLINE _Wrapped' #-}

p1apply :: Num a => Transformation V1 a -> a -> a
p1apply t a = papply t (P (V1 a)) ^. _x

instance Transformable ColourMap where
  transform t = over (_Wrapped' . _Wrapped' . mapped . _1) (p1apply t)

type instance Index ColourMap   = Rational
type instance IxValue ColourMap = AlphaColour Double

instance AsEmpty ColourMap where
  _Empty = nearly (ColourMap M.empty) (allOf each (==transparent))

instance Each ColourMap ColourMap (AlphaColour Double) (AlphaColour Double) where
  each = _Wrapped . each

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
              in  blend a c1 c2
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

