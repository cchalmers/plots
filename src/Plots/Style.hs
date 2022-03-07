{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE ViewPatterns           #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Plots.Style
-- Copyright   :  (C) 2016 Christopher Chalmers
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
  , ixColourR
  , cmTraverse
  , colourMap
  , colourList
  , toStops

  , NanColours
  , HasNanColours (..)

    -- ** Sample maps
  , viridis
  , magma
  , inferno
  , plasma
  , greys

  ) where

import qualified Control.Lens     as Lens
import           Data.Colour.SRGB
import qualified Data.Map         as M
import           Data.Typeable
import           Diagrams.Prelude hiding (magma)
import           Linear

-- | Plot styles are used to style each plot in an axis. Every 'Axis'
--   comes with a list of plots styles (contained in the 'AxisStyle')
--   which get applied the plots upon rendering.
--
--   You can either change the list of plot styles used with
--   'axisStyle':
--
-- @
-- stylishAxis = r2Axis &~ do
--   axisStyle .= vividColours
--   linePlot [(1,2) (3,4)] $ key "line 1"
--   linePlot [(1,1) (4,2)] $ key "line 2"
-- @
--
--   change the style for individual plots when changing the plot state.
--
-- @
-- stylishAxis2 = r2Axis &~ do
--   linePlot [(1,2) (3,4)] $ do
--     key "line 1"
--     plotColour .= green
--   linePlot [(1,1) (4,2)] $ do
--     key "line 2"
--     plotColour .= orange
-- @
--
--   A plot style is made up of separate styles ('lineStyle',
--   'markerStyle', 'areaStyle' and 'textStyle') a 'plotColour' and a
--   'plotMarker'. When rendering a plot, the 'PlotStyle's in an
--   'AxisStyle' are used to style each plot. The lenses can be used to
--   customise each style when adding the plot.
--
--   * 'plotColour' - the underlying colour of the plot
--   * 'lineStyle' - style used for lines ('linePlot', 'connectingLine'
--      in a 'scatterPlot')
--   * 'areaStyle' - style used for any area ('barPlot', 'piePlot',
--      'histogramPlot')
--   * 'markerStyle' - style used for markers in 'scatterPlot'
--   * 'plotMarker' - marker used in 'scatterPlot'
data PlotStyle b v n = PlotStyle
  { _plotColour  :: Colour Double
  , _lineStyle   :: Colour Double -> Style v n
  , _markerStyle :: Colour Double -> Style v n
  , _areaStyle   :: Colour Double -> Style v n
  , _textStyle   :: Colour Double -> Style v n
  , _plotMarker  :: QDiagram b v n Any
  } deriving Typeable
  -- XXX link to examples in haddock?

type instance V (PlotStyle b v n) = v
type instance N (PlotStyle b v n) = n

-- | Class for objects that contain a 'PlotStyle'.
class HasPlotStyle f a b | a -> b where
  -- | Lens onto the 'PlotStyle'.
  plotStyle :: LensLike' f a (PlotStyle b (V a) (N a))

  -- | The 'plotColour' is the overall colour of the plot. This is passed
  --   to the other styles ('lineStyle', 'markerStyle' etc.) to give an
  --   overall colour for the plot.
  plotColour :: Functor f => LensLike' f a (Colour Double)
  plotColour = plotStyle . lens _plotColour (\p f -> p {_plotColour = f})

  -- | Alias for 'plotColour'.
  plotColor :: Functor f => LensLike' f a (Colour Double)
  plotColor = plotColour

  -- | This style is applied to any plots made up of lines only (like
  --   'Path' plots). This is a less general version of
  --   'lineStyleFunction'.
  lineStyle :: Settable f => LensLike' f a (Style (V a) (N a))
  lineStyle = lineStyleFunction . mapped

  -- | A version 'lineStyle' with access to the current 'plotColour'
  --   when 'applyLineStyle' is used.
  lineStyleFunction :: Functor f => LensLike' f a (Colour Double ->
    Style (V a) (N a))
  lineStyleFunction = plotStyle . lens _lineStyle (\p f -> p {_lineStyle = f})

  -- | This style is applied to any markers in the plot (usually the
  --   'plotMarker'). This is a less general version of
  --   'markerStyleFunction'.
  markerStyle :: Settable f => LensLike' f a (Style (V a) (N a))
  markerStyle = markerStyleFunction . mapped

  -- | A version 'lineStyle' with access to the current 'plotColour' when
  --   'applyMarkerStyle' is used.
  markerStyleFunction :: Functor f => LensLike' f a (Colour Double -> Style (V a) (N a))
  markerStyleFunction = plotStyle . lens _markerStyle (\p f -> p {_markerStyle = f})

  -- | This style is applied to any filled areas in a plot (like
  --   'Plots.Types.Bar' or 'Plots.Styles.Ribbon'). This is a less
  --   general version of 'areaStyleFunction'.
  areaStyle :: Settable f => LensLike' f a (Style (V a) (N a))
  areaStyle = areaStyleFunction . mapped

  -- | A version 'areaStyle' with access to the current 'plotColour' when
  --   'applyAreaStyle' is used.
  areaStyleFunction :: Functor f => LensLike' f a (Colour Double -> Style (V a) (N a))
  areaStyleFunction = plotStyle . lens _areaStyle (\p f -> p {_areaStyle = f})

  -- | This style is applied to text plots. This is a less general
  --   version of 'textStyleFunction'.
  textStyle :: Settable f => LensLike' f a (Style (V a) (N a))
  textStyle = textStyleFunction . mapped

  -- | A version 'textStyle' with access to the current 'plotColour' when
  --   'applyAreaStyle' is used.
  textStyleFunction :: Functor f => LensLike' f a (Colour Double -> Style (V a) (N a))
  textStyleFunction = plotStyle . lens _textStyle (\p f -> p {_textStyle = f})

  -- | This diagram is used as any markers in a plot (like
  --   'Plots.Types.Scatter'). The 'markerStyle' will be applied to this
  --   marker when the plot gets rendered.
  plotMarker :: Functor f => LensLike' f a (QDiagram b (V a) (N a) Any)
  plotMarker = plotStyle . lens _plotMarker (\p f -> p {_plotMarker = f})

  -- | A traversal over all the styles ('lineStyle', 'markerStyle',
  --  'areaStyle' and 'textStyle') of a 'PlotStyle'. This is a less
  --  general version of 'plotStyleFunctions'.
  plotStyles :: Settable f => LensLike' f a (Style (V a) (N a))
  plotStyles = plotStyleFunctions . mapped

  -- | A version of 'plotStyles' with access to the 'plotColour'.
  plotStyleFunctions :: Applicative f => LensLike' f a (Colour Double -> Style (V a) (N a))
  plotStyleFunctions = plotStyle . t
    where
      t f PlotStyle {..} = PlotStyle
        <$> pure _plotColour
        <*> f _lineStyle
        <*> f _markerStyle
        <*> f _areaStyle
        <*> f _textStyle
        <*> pure _plotMarker

instance HasPlotStyle f (PlotStyle b v n) b where
  plotStyle = id

-- Applying styles -----------------------------------------------------

-- | Apply the 'lineStyle' from a 'PlotStyle'.
--
-- @
-- applyLineStyle :: (InSpace v n t, HasStyle t) => PlotStyle b v n -> t -> t
-- @
applyLineStyle
  :: (SameSpace a t, HasPlotStyle (Const (PlotStyle b (V a) (N a))) a b, HasStyle t)
  => a -> t -> t
applyLineStyle (view plotStyle -> sty) =
  applyStyle $ (sty ^. lineStyleFunction) (sty ^. plotColour)

-- | Apply the 'markerStyle' from a 'PlotStyle'.
--
-- @
-- applyMarkerStyle :: (InSpace v n t, HasStyle t) => PlotStyle b v n -> t -> t
-- @
applyMarkerStyle
  :: (SameSpace a t, HasPlotStyle (Const (PlotStyle b (V a) (N a))) a b, HasStyle t)
  => a -> t -> t
applyMarkerStyle (view plotStyle -> sty) =
  applyStyle $ (sty ^. markerStyleFunction) (sty ^. plotColour)

-- | Apply the 'areaStyle from a 'PlotStyle'.
--
-- @
-- applyLineStyle :: (InSpace v n t, HasStyle t) => PlotStyle b v n -> t -> t
-- @
applyAreaStyle
  :: (SameSpace a t, HasPlotStyle (Const (PlotStyle b (V a) (N a))) a b, HasStyle t)
  => a -> t -> t
applyAreaStyle (view plotStyle -> sty) =
  applyStyle $ (sty ^. areaStyleFunction) (sty ^. plotColour)

-- | Apply the 'textStyle' from a 'PlotStyle'.
--
-- @
-- applyTextStyle :: (InSpace v n t, HasStyle t) => PlotStyle b v n -> t -> t
-- @
applyTextStyle
  :: (SameSpace a t, HasPlotStyle (Const (PlotStyle b (V a) (N a))) a b, HasStyle t)
  => a -> t -> t
applyTextStyle (view plotStyle -> sty) =
  applyStyle $ (sty ^. textStyleFunction) (sty ^. plotColour)

instance (Metric v, Traversable v, OrderedField n) => Transformable (PlotStyle b v n) where
  transform t = (plotMarker %~ transform t) . (plotStyles %~ transform t)

------------------------------------------------------------------------
-- Axis Style
------------------------------------------------------------------------

-- | The 'AxisStyle' determines the 'Style's of the plots in an axis.
--   There are various predefined styles to change the look of the plot.
data AxisStyle b v n = AxisStyle ColourMap [PlotStyle b v n]

type instance V (AxisStyle b v n) = v
type instance N (AxisStyle b v n) = n

-- | Class of things that have an 'AxisStyle'.
class HasAxisStyle a b | a -> b where
  -- | Lens onto the 'AxisStyle'.
  axisStyle :: Lens' a (AxisStyle b (V a) (N a))

  -- | The 'ColourMap' is used to draw the 'Plots.Axis.ColourBar' and
  --   render plots like 'Plots.HeatMap'.
  axisColourMap :: Lens' a ColourMap
  axisColourMap = axisStyle . cm
    where cm f (AxisStyle c ss) = f c <&> \c' -> AxisStyle c' ss

  -- | Traversal over the 'PlotStyle's in an 'AxisStyle'. There are always
  --   an infinite number of 'PlotStyle's in an 'AxisStyle'.
  axisStyles :: IndexedTraversal' Int a (PlotStyle b (V a) (N a))
  axisStyles = axisStyle . stys . traversed
    where stys f (AxisStyle c ss) = f ss <&> \ss' -> AxisStyle c ss'

instance HasAxisStyle (AxisStyle b v n) b where
  axisStyle = id

instance Applicative f => HasPlotStyle f (AxisStyle b v n) b where
  plotStyle = axisStyles

------------------------------------------------------------------------
-- Predefined themes
------------------------------------------------------------------------

-- $predefined
-- There are only a few themes for now. Eventually there will be a wider
-- range of themes with better support for customisation.

-- code to display axis styles
-- > import Plots
-- > showcase :: AxisStyle B V2 Double -> Diagram B
-- > showcase sty = hsep 0.4 . take 8 $ map drawSty stys
-- >   where
-- >     stys = sty ^.. axisStyles
-- >     drawSty s = applyMarkerStyle s (s^.plotMarker) <> applyLineStyle s lineBehind
-- >     lineBehind = (p2 (-1,0) ~~ p2 (1,0))
--
-- > fadedColourPic   = showcase fadedColours
-- > vividColourPic   = showcase vividColours
-- > blackAndWhitePic = showcase blackAndWhite

-- | Theme using 'funColours' with faded fills and thick lines.
--
-- <<diagrams/src_Plots_Style_fadedColourPic.svg#diagram=fadedColourPic&width=600>>
fadedColours :: (TypeableFloat n, Renderable (Path V2 n) b) => AxisStyle b V2 n
fadedColours = AxisStyle viridis $
  zipWith mkStyle (cycle colours1) (cycle $ map stroke filledMarkers)
  where
    mkStyle c = PlotStyle c lineS fadeS fadeS fillS
    lineS c = mempty # lc c # lwO 3 --  normal
    fadeS c = mempty # fc (blend 0.1 white c) # lc c # lwO 1
    fillS c = mempty # fc c # lw none

-- | Theme using 'funColours' with no lines on 'areaStyle.
--
-- <<diagrams/src_Plots_Style_vividColourPic.svg#diagram=vividColourPic&width=600>>
vividColours :: (TypeableFloat n, Renderable (Path V2 n) b) => AxisStyle b V2 n
vividColours = AxisStyle viridis $
  zipWith mkStyle (cycle colours2) (cycle $ map (scale 1.2 . stroke) filledMarkers)
  where
    mkStyle c = PlotStyle c lineS markS fillS fillS
    lineS c = mempty # lc c # lwO 3 -- normal
    markS c = mempty # fc c # lwO 1 # lc white
    fillS c = mempty # fc c # lw none

-- | Theme without any colours, useful for black and white documents.
--
-- <<diagrams/src_Plots_Style_blackAndWhitePic.svg#diagram=blackAndWhitePic&width=600>>
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
filledMarkers = scale 11 . map (centerXY . pathFromTrail) $ cycle
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
lineMarkers = scale 11 $ cycle
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

------------------------------------------------------------------------
-- Colour maps
------------------------------------------------------------------------

-- | Colours to use when representing @NaN@, @Infinity@ and @-Infinity@.
data NanColours = NanColours
  { _nanColour    :: Colour Double
  , _infColour    :: Colour Double
  , _negInfColour :: Colour Double
  } deriving Show

class HasNanColours a where
  -- | Colours to use when displaying @NaN@, @Infinity@ and @-Infinity@.
  nanColours :: Lens' a NanColours

  -- | Colour to use when displaying @NaN@.
  --
  -- Default is 'white.
  nanColour :: Lens' a (Colour Double)
  nanColour = nanColours . lens _nanColour (\n c -> n {_nanColour = c})

  -- | Colour to use when displaying @Infinity@.
  --
  -- Default is 'lime'.
  infColour :: Lens' a (Colour Double)
  infColour = nanColours . lens _infColour (\n c -> n {_infColour = c})

  -- | Colour to use when displaying @-Infinity@.
  --
  -- Default is 'magenta'.
  negInfColour :: Lens' a (Colour Double)
  negInfColour = nanColours . lens _negInfColour (\n c -> n {_negInfColour = c})

instance HasNanColours NanColours where
  nanColours = id

instance Default NanColours where
  def = NanColours
    { _nanColour    = magenta
    , _infColour    = white
    , _negInfColour = black
    }

-- type ColourMap = [(Double, Colour Double)]

-- | A map from a number (usually between 0 and 1) to a colour. Colour
--   maps are part of the 'AxisStyle', which is used for plots like
--   'Plots.Types.HeatMap'.
data ColourMap = ColourMap NanColours (M.Map Rational (Colour Double))
  deriving Show

type instance V ColourMap = V1
type instance N ColourMap = Rational

cmap :: Lens' ColourMap (M.Map Rational (Colour Double))
cmap f (ColourMap ncs cs) = f cs <&> \cs' -> ColourMap ncs cs'

instance HasNanColours ColourMap where
  nanColours f (ColourMap ncs cs) = f ncs <&> \ncs' -> ColourMap ncs' cs

p1apply :: Num a => Transformation V1 a -> a -> a
p1apply t a = papply t (P (V1 a)) ^. _x

instance Transformable ColourMap where
  transform t = over (cmap . _Wrapped' . mapped . _1) (p1apply t)

type instance Index ColourMap   = Rational
type instance IxValue ColourMap = Colour Double

instance Each ColourMap ColourMap (Colour Double) (Colour Double) where
  each = cmap . each

instance Ixed ColourMap where
  ix x = ixColourR x

-- | 'Nothing' == 'transparent'
instance At ColourMap where
  at x = ixColourR x . from (non black)

ixColour :: Double -> Lens' ColourMap (Colour Double)
ixColour x f cM@(ColourMap ncs cm)
  | isNaN x      = nanColour f cM
  | isInfinite x = if x < 0 then negInfColour f cM else infColour f cM
  | otherwise    = f c <&> \c' -> ColourMap ncs (M.insert r c' cm)
  where
  r = toRational x
  c = case (M.lookupLE r cm, M.lookupGE r cm) of
        (Just (i,c1), Just (j,c2))
          | i == j    -> c1
          | otherwise ->
              let a = fromRational $ (r - i) / (j - i)
              in  blend a c2 c1
        (Just (_,c1), Nothing) -> c1
        (Nothing, Just (_,c2)) -> c2
        _                      -> black

ixColourR :: Rational -> Lens' ColourMap (Colour Double)
ixColourR x f (ColourMap ncs cm) = f c <&> \c' -> ColourMap ncs (M.insert x c' cm)
  where
  c = case (M.lookupLE x cm, M.lookupGE x cm) of
        (Just (i,c1), Just (j,c2))
          | i == j    -> c1
          | otherwise ->
              let a = fromRational $ (x - i) / (j - i)
              in  blend a c2 c1
        (Just (_,c1), Nothing) -> c1
        (Nothing, Just (_,c2)) -> c2
        _                      -> black

-- | Indexed traversal over the colours indexed and ordered by their
--   position in the map.
cmTraverse :: IndexedTraversal' Rational ColourMap (Colour Double)
cmTraverse = cmap . itraversed

-- | Return the list of colours in the [0,1] range in order. This always
--   includes colours 0 and 1.
colourList :: ColourMap -> [(Rational, Colour Double)]
colourList = itoListOf (cmTraverse . ifiltered (\i _ -> i >= 0 && i <= 1))
           . (ixColourR 0 %~ id) . (ixColourR 1 %~ id)
           -- touch colours at 0 and 1 so they're in the list

colourMap :: [(Rational, Colour Double)] -> ColourMap
colourMap [] = ColourMap def M.empty
colourMap cs
  | a == b        = ColourMap def (M.singleton 0.5 c)
  | otherwise     = ColourMap def (M.mapKeysMonotonic normalise cm)
  where
    cm    = M.fromList cs
    (a,c) = M.findMin cm
    (b,_) = M.findMax cm
    normalise x = (x - a) / (b - a)

toStops :: Fractional n => ColourMap -> [GradientStop n]
toStops = map (\(x,c) -> GradientStop (SomeColor c) (fromRational x))
        . colourList

-- Colour maps ---------------------------------------------------------

-- > import Plots
-- > cmBar cm = gradientColourBar cm # scaleX 15
-- > greysBar = cmBar greys
-- > magmaBar = cmBar Plots.magma
-- > infernoBar = cmBar inferno
-- > plasmaBar = cmBar plasma
-- > viridisBar = cmBar viridis

-- | A colour map from black to white.
--
-- <<diagrams/src_Plots_Style_greysBar.svg#diagram=greysBar&width=600>>
greys :: ColourMap
greys = colourMap [(0, black), (1, white)]

-- | The magma colour map taken from https://bids.github.io/colormap/.
--
-- <<diagrams/src_Plots_Style_magmaBar.svg#diagram=magmaBar&width=600>>
magma :: ColourMap
magma = colourMap $ zip [1..]
  [ sRGB 0.001462 0.000466 0.013866, sRGB 0.002258 0.001295 0.018331
  , sRGB 0.003279 0.002305 0.023708, sRGB 0.004512 0.003490 0.029965
  , sRGB 0.005950 0.004843 0.037130, sRGB 0.007588 0.006356 0.044973
  , sRGB 0.009426 0.008022 0.052844, sRGB 0.011465 0.009828 0.060750
  , sRGB 0.013708 0.011771 0.068667, sRGB 0.016156 0.013840 0.076603
  , sRGB 0.018815 0.016026 0.084584, sRGB 0.021692 0.018320 0.092610
  , sRGB 0.024792 0.020715 0.100676, sRGB 0.028123 0.023201 0.108787
  , sRGB 0.031696 0.025765 0.116965, sRGB 0.035520 0.028397 0.125209
  , sRGB 0.039608 0.031090 0.133515, sRGB 0.043830 0.033830 0.141886
  , sRGB 0.048062 0.036607 0.150327, sRGB 0.052320 0.039407 0.158841
  , sRGB 0.056615 0.042160 0.167446, sRGB 0.060949 0.044794 0.176129
  , sRGB 0.065330 0.047318 0.184892, sRGB 0.069764 0.049726 0.193735
  , sRGB 0.074257 0.052017 0.202660, sRGB 0.078815 0.054184 0.211667
  , sRGB 0.083446 0.056225 0.220755, sRGB 0.088155 0.058133 0.229922
  , sRGB 0.092949 0.059904 0.239164, sRGB 0.097833 0.061531 0.248477
  , sRGB 0.102815 0.063010 0.257854, sRGB 0.107899 0.064335 0.267289
  , sRGB 0.113094 0.065492 0.276784, sRGB 0.118405 0.066479 0.286321
  , sRGB 0.123833 0.067295 0.295879, sRGB 0.129380 0.067935 0.305443
  , sRGB 0.135053 0.068391 0.315000, sRGB 0.140858 0.068654 0.324538
  , sRGB 0.146785 0.068738 0.334011, sRGB 0.152839 0.068637 0.343404
  , sRGB 0.159018 0.068354 0.352688, sRGB 0.165308 0.067911 0.361816
  , sRGB 0.171713 0.067305 0.370771, sRGB 0.178212 0.066576 0.379497
  , sRGB 0.184801 0.065732 0.387973, sRGB 0.191460 0.064818 0.396152
  , sRGB 0.198177 0.063862 0.404009, sRGB 0.204935 0.062907 0.411514
  , sRGB 0.211718 0.061992 0.418647, sRGB 0.218512 0.061158 0.425392
  , sRGB 0.225302 0.060445 0.431742, sRGB 0.232077 0.059889 0.437695
  , sRGB 0.238826 0.059517 0.443256, sRGB 0.245543 0.059352 0.448436
  , sRGB 0.252220 0.059415 0.453248, sRGB 0.258857 0.059706 0.457710
  , sRGB 0.265447 0.060237 0.461840, sRGB 0.271994 0.060994 0.465660
  , sRGB 0.278493 0.061978 0.469190, sRGB 0.284951 0.063168 0.472451
  , sRGB 0.291366 0.064553 0.475462, sRGB 0.297740 0.066117 0.478243
  , sRGB 0.304081 0.067835 0.480812, sRGB 0.310382 0.069702 0.483186
  , sRGB 0.316654 0.071690 0.485380, sRGB 0.322899 0.073782 0.487408
  , sRGB 0.329114 0.075972 0.489287, sRGB 0.335308 0.078236 0.491024
  , sRGB 0.341482 0.080564 0.492631, sRGB 0.347636 0.082946 0.494121
  , sRGB 0.353773 0.085373 0.495501, sRGB 0.359898 0.087831 0.496778
  , sRGB 0.366012 0.090314 0.497960, sRGB 0.372116 0.092816 0.499053
  , sRGB 0.378211 0.095332 0.500067, sRGB 0.384299 0.097855 0.501002
  , sRGB 0.390384 0.100379 0.501864, sRGB 0.396467 0.102902 0.502658
  , sRGB 0.402548 0.105420 0.503386, sRGB 0.408629 0.107930 0.504052
  , sRGB 0.414709 0.110431 0.504662, sRGB 0.420791 0.112920 0.505215
  , sRGB 0.426877 0.115395 0.505714, sRGB 0.432967 0.117855 0.506160
  , sRGB 0.439062 0.120298 0.506555, sRGB 0.445163 0.122724 0.506901
  , sRGB 0.451271 0.125132 0.507198, sRGB 0.457386 0.127522 0.507448
  , sRGB 0.463508 0.129893 0.507652, sRGB 0.469640 0.132245 0.507809
  , sRGB 0.475780 0.134577 0.507921, sRGB 0.481929 0.136891 0.507989
  , sRGB 0.488088 0.139186 0.508011, sRGB 0.494258 0.141462 0.507988
  , sRGB 0.500438 0.143719 0.507920, sRGB 0.506629 0.145958 0.507806
  , sRGB 0.512831 0.148179 0.507648, sRGB 0.519045 0.150383 0.507443
  , sRGB 0.525270 0.152569 0.507192, sRGB 0.531507 0.154739 0.506895
  , sRGB 0.537755 0.156894 0.506551, sRGB 0.544015 0.159033 0.506159
  , sRGB 0.550287 0.161158 0.505719, sRGB 0.556571 0.163269 0.505230
  , sRGB 0.562866 0.165368 0.504692, sRGB 0.569172 0.167454 0.504105
  , sRGB 0.575490 0.169530 0.503466, sRGB 0.581819 0.171596 0.502777
  , sRGB 0.588158 0.173652 0.502035, sRGB 0.594508 0.175701 0.501241
  , sRGB 0.600868 0.177743 0.500394, sRGB 0.607238 0.179779 0.499492
  , sRGB 0.613617 0.181811 0.498536, sRGB 0.620005 0.183840 0.497524
  , sRGB 0.626401 0.185867 0.496456, sRGB 0.632805 0.187893 0.495332
  , sRGB 0.639216 0.189921 0.494150, sRGB 0.645633 0.191952 0.492910
  , sRGB 0.652056 0.193986 0.491611, sRGB 0.658483 0.196027 0.490253
  , sRGB 0.664915 0.198075 0.488836, sRGB 0.671349 0.200133 0.487358
  , sRGB 0.677786 0.202203 0.485819, sRGB 0.684224 0.204286 0.484219
  , sRGB 0.690661 0.206384 0.482558, sRGB 0.697098 0.208501 0.480835
  , sRGB 0.703532 0.210638 0.479049, sRGB 0.709962 0.212797 0.477201
  , sRGB 0.716387 0.214982 0.475290, sRGB 0.722805 0.217194 0.473316
  , sRGB 0.729216 0.219437 0.471279, sRGB 0.735616 0.221713 0.469180
  , sRGB 0.742004 0.224025 0.467018, sRGB 0.748378 0.226377 0.464794
  , sRGB 0.754737 0.228772 0.462509, sRGB 0.761077 0.231214 0.460162
  , sRGB 0.767398 0.233705 0.457755, sRGB 0.773695 0.236249 0.455289
  , sRGB 0.779968 0.238851 0.452765, sRGB 0.786212 0.241514 0.450184
  , sRGB 0.792427 0.244242 0.447543, sRGB 0.798608 0.247040 0.444848
  , sRGB 0.804752 0.249911 0.442102, sRGB 0.810855 0.252861 0.439305
  , sRGB 0.816914 0.255895 0.436461, sRGB 0.822926 0.259016 0.433573
  , sRGB 0.828886 0.262229 0.430644, sRGB 0.834791 0.265540 0.427671
  , sRGB 0.840636 0.268953 0.424666, sRGB 0.846416 0.272473 0.421631
  , sRGB 0.852126 0.276106 0.418573, sRGB 0.857763 0.279857 0.415496
  , sRGB 0.863320 0.283729 0.412403, sRGB 0.868793 0.287728 0.409303
  , sRGB 0.874176 0.291859 0.406205, sRGB 0.879464 0.296125 0.403118
  , sRGB 0.884651 0.300530 0.400047, sRGB 0.889731 0.305079 0.397002
  , sRGB 0.894700 0.309773 0.393995, sRGB 0.899552 0.314616 0.391037
  , sRGB 0.904281 0.319610 0.388137, sRGB 0.908884 0.324755 0.385308
  , sRGB 0.913354 0.330052 0.382563, sRGB 0.917689 0.335500 0.379915
  , sRGB 0.921884 0.341098 0.377376, sRGB 0.925937 0.346844 0.374959
  , sRGB 0.929845 0.352734 0.372677, sRGB 0.933606 0.358764 0.370541
  , sRGB 0.937221 0.364929 0.368567, sRGB 0.940687 0.371224 0.366762
  , sRGB 0.944006 0.377643 0.365136, sRGB 0.947180 0.384178 0.363701
  , sRGB 0.950210 0.390820 0.362468, sRGB 0.953099 0.397563 0.361438
  , sRGB 0.955849 0.404400 0.360619, sRGB 0.958464 0.411324 0.360014
  , sRGB 0.960949 0.418323 0.359630, sRGB 0.963310 0.425390 0.359469
  , sRGB 0.965549 0.432519 0.359529, sRGB 0.967671 0.439703 0.359810
  , sRGB 0.969680 0.446936 0.360311, sRGB 0.971582 0.454210 0.361030
  , sRGB 0.973381 0.461520 0.361965, sRGB 0.975082 0.468861 0.363111
  , sRGB 0.976690 0.476226 0.364466, sRGB 0.978210 0.483612 0.366025
  , sRGB 0.979645 0.491014 0.367783, sRGB 0.981000 0.498428 0.369734
  , sRGB 0.982279 0.505851 0.371874, sRGB 0.983485 0.513280 0.374198
  , sRGB 0.984622 0.520713 0.376698, sRGB 0.985693 0.528148 0.379371
  , sRGB 0.986700 0.535582 0.382210, sRGB 0.987646 0.543015 0.385210
  , sRGB 0.988533 0.550446 0.388365, sRGB 0.989363 0.557873 0.391671
  , sRGB 0.990138 0.565296 0.395122, sRGB 0.990871 0.572706 0.398714
  , sRGB 0.991558 0.580107 0.402441, sRGB 0.992196 0.587502 0.406299
  , sRGB 0.992785 0.594891 0.410283, sRGB 0.993326 0.602275 0.414390
  , sRGB 0.993834 0.609644 0.418613, sRGB 0.994309 0.616999 0.422950
  , sRGB 0.994738 0.624350 0.427397, sRGB 0.995122 0.631696 0.431951
  , sRGB 0.995480 0.639027 0.436607, sRGB 0.995810 0.646344 0.441361
  , sRGB 0.996096 0.653659 0.446213, sRGB 0.996341 0.660969 0.451160
  , sRGB 0.996580 0.668256 0.456192, sRGB 0.996775 0.675541 0.461314
  , sRGB 0.996925 0.682828 0.466526, sRGB 0.997077 0.690088 0.471811
  , sRGB 0.997186 0.697349 0.477182, sRGB 0.997254 0.704611 0.482635
  , sRGB 0.997325 0.711848 0.488154, sRGB 0.997351 0.719089 0.493755
  , sRGB 0.997351 0.726324 0.499428, sRGB 0.997341 0.733545 0.505167
  , sRGB 0.997285 0.740772 0.510983, sRGB 0.997228 0.747981 0.516859
  , sRGB 0.997138 0.755190 0.522806, sRGB 0.997019 0.762398 0.528821
  , sRGB 0.996898 0.769591 0.534892, sRGB 0.996727 0.776795 0.541039
  , sRGB 0.996571 0.783977 0.547233, sRGB 0.996369 0.791167 0.553499
  , sRGB 0.996162 0.798348 0.559820, sRGB 0.995932 0.805527 0.566202
  , sRGB 0.995680 0.812706 0.572645, sRGB 0.995424 0.819875 0.579140
  , sRGB 0.995131 0.827052 0.585701, sRGB 0.994851 0.834213 0.592307
  , sRGB 0.994524 0.841387 0.598983, sRGB 0.994222 0.848540 0.605696
  , sRGB 0.993866 0.855711 0.612482, sRGB 0.993545 0.862859 0.619299
  , sRGB 0.993170 0.870024 0.626189, sRGB 0.992831 0.877168 0.633109
  , sRGB 0.992440 0.884330 0.640099, sRGB 0.992089 0.891470 0.647116
  , sRGB 0.991688 0.898627 0.654202, sRGB 0.991332 0.905763 0.661309
  , sRGB 0.990930 0.912915 0.668481, sRGB 0.990570 0.920049 0.675675
  , sRGB 0.990175 0.927196 0.682926, sRGB 0.989815 0.934329 0.690198
  , sRGB 0.989434 0.941470 0.697519, sRGB 0.989077 0.948604 0.704863
  , sRGB 0.988717 0.955742 0.712242, sRGB 0.988367 0.962878 0.719649
  , sRGB 0.988033 0.970012 0.727077, sRGB 0.987691 0.977154 0.734536
  , sRGB 0.987387 0.984288 0.742002, sRGB 0.987053 0.991438 0.749504
  ]

-- | The inferno colour map taken from https://bids.github.io/colormap/.
--
-- <<diagrams/src_Plots_Style_infernoBar.svg#diagram=infernoBar&width=600>>
inferno :: ColourMap
inferno = colourMap $ zip [1..]
  [ sRGB 0.001462 0.000466 0.013866, sRGB 0.002267 0.001270 0.018570
  , sRGB 0.003299 0.002249 0.024239, sRGB 0.004547 0.003392 0.030909
  , sRGB 0.006006 0.004692 0.038558, sRGB 0.007676 0.006136 0.046836
  , sRGB 0.009561 0.007713 0.055143, sRGB 0.011663 0.009417 0.063460
  , sRGB 0.013995 0.011225 0.071862, sRGB 0.016561 0.013136 0.080282
  , sRGB 0.019373 0.015133 0.088767, sRGB 0.022447 0.017199 0.097327
  , sRGB 0.025793 0.019331 0.105930, sRGB 0.029432 0.021503 0.114621
  , sRGB 0.033385 0.023702 0.123397, sRGB 0.037668 0.025921 0.132232
  , sRGB 0.042253 0.028139 0.141141, sRGB 0.046915 0.030324 0.150164
  , sRGB 0.051644 0.032474 0.159254, sRGB 0.056449 0.034569 0.168414
  , sRGB 0.061340 0.036590 0.177642, sRGB 0.066331 0.038504 0.186962
  , sRGB 0.071429 0.040294 0.196354, sRGB 0.076637 0.041905 0.205799
  , sRGB 0.081962 0.043328 0.215289, sRGB 0.087411 0.044556 0.224813
  , sRGB 0.092990 0.045583 0.234358, sRGB 0.098702 0.046402 0.243904
  , sRGB 0.104551 0.047008 0.253430, sRGB 0.110536 0.047399 0.262912
  , sRGB 0.116656 0.047574 0.272321, sRGB 0.122908 0.047536 0.281624
  , sRGB 0.129285 0.047293 0.290788, sRGB 0.135778 0.046856 0.299776
  , sRGB 0.142378 0.046242 0.308553, sRGB 0.149073 0.045468 0.317085
  , sRGB 0.155850 0.044559 0.325338, sRGB 0.162689 0.043554 0.333277
  , sRGB 0.169575 0.042489 0.340874, sRGB 0.176493 0.041402 0.348111
  , sRGB 0.183429 0.040329 0.354971, sRGB 0.190367 0.039309 0.361447
  , sRGB 0.197297 0.038400 0.367535, sRGB 0.204209 0.037632 0.373238
  , sRGB 0.211095 0.037030 0.378563, sRGB 0.217949 0.036615 0.383522
  , sRGB 0.224763 0.036405 0.388129, sRGB 0.231538 0.036405 0.392400
  , sRGB 0.238273 0.036621 0.396353, sRGB 0.244967 0.037055 0.400007
  , sRGB 0.251620 0.037705 0.403378, sRGB 0.258234 0.038571 0.406485
  , sRGB 0.264810 0.039647 0.409345, sRGB 0.271347 0.040922 0.411976
  , sRGB 0.277850 0.042353 0.414392, sRGB 0.284321 0.043933 0.416608
  , sRGB 0.290763 0.045644 0.418637, sRGB 0.297178 0.047470 0.420491
  , sRGB 0.303568 0.049396 0.422182, sRGB 0.309935 0.051407 0.423721
  , sRGB 0.316282 0.053490 0.425116, sRGB 0.322610 0.055634 0.426377
  , sRGB 0.328921 0.057827 0.427511, sRGB 0.335217 0.060060 0.428524
  , sRGB 0.341500 0.062325 0.429425, sRGB 0.347771 0.064616 0.430217
  , sRGB 0.354032 0.066925 0.430906, sRGB 0.360284 0.069247 0.431497
  , sRGB 0.366529 0.071579 0.431994, sRGB 0.372768 0.073915 0.432400
  , sRGB 0.379001 0.076253 0.432719, sRGB 0.385228 0.078591 0.432955
  , sRGB 0.391453 0.080927 0.433109, sRGB 0.397674 0.083257 0.433183
  , sRGB 0.403894 0.085580 0.433179, sRGB 0.410113 0.087896 0.433098
  , sRGB 0.416331 0.090203 0.432943, sRGB 0.422549 0.092501 0.432714
  , sRGB 0.428768 0.094790 0.432412, sRGB 0.434987 0.097069 0.432039
  , sRGB 0.441207 0.099338 0.431594, sRGB 0.447428 0.101597 0.431080
  , sRGB 0.453651 0.103848 0.430498, sRGB 0.459875 0.106089 0.429846
  , sRGB 0.466100 0.108322 0.429125, sRGB 0.472328 0.110547 0.428334
  , sRGB 0.478558 0.112764 0.427475, sRGB 0.484789 0.114974 0.426548
  , sRGB 0.491022 0.117179 0.425552, sRGB 0.497257 0.119379 0.424488
  , sRGB 0.503493 0.121575 0.423356, sRGB 0.509730 0.123769 0.422156
  , sRGB 0.515967 0.125960 0.420887, sRGB 0.522206 0.128150 0.419549
  , sRGB 0.528444 0.130341 0.418142, sRGB 0.534683 0.132534 0.416667
  , sRGB 0.540920 0.134729 0.415123, sRGB 0.547157 0.136929 0.413511
  , sRGB 0.553392 0.139134 0.411829, sRGB 0.559624 0.141346 0.410078
  , sRGB 0.565854 0.143567 0.408258, sRGB 0.572081 0.145797 0.406369
  , sRGB 0.578304 0.148039 0.404411, sRGB 0.584521 0.150294 0.402385
  , sRGB 0.590734 0.152563 0.400290, sRGB 0.596940 0.154848 0.398125
  , sRGB 0.603139 0.157151 0.395891, sRGB 0.609330 0.159474 0.393589
  , sRGB 0.615513 0.161817 0.391219, sRGB 0.621685 0.164184 0.388781
  , sRGB 0.627847 0.166575 0.386276, sRGB 0.633998 0.168992 0.383704
  , sRGB 0.640135 0.171438 0.381065, sRGB 0.646260 0.173914 0.378359
  , sRGB 0.652369 0.176421 0.375586, sRGB 0.658463 0.178962 0.372748
  , sRGB 0.664540 0.181539 0.369846, sRGB 0.670599 0.184153 0.366879
  , sRGB 0.676638 0.186807 0.363849, sRGB 0.682656 0.189501 0.360757
  , sRGB 0.688653 0.192239 0.357603, sRGB 0.694627 0.195021 0.354388
  , sRGB 0.700576 0.197851 0.351113, sRGB 0.706500 0.200728 0.347777
  , sRGB 0.712396 0.203656 0.344383, sRGB 0.718264 0.206636 0.340931
  , sRGB 0.724103 0.209670 0.337424, sRGB 0.729909 0.212759 0.333861
  , sRGB 0.735683 0.215906 0.330245, sRGB 0.741423 0.219112 0.326576
  , sRGB 0.747127 0.222378 0.322856, sRGB 0.752794 0.225706 0.319085
  , sRGB 0.758422 0.229097 0.315266, sRGB 0.764010 0.232554 0.311399
  , sRGB 0.769556 0.236077 0.307485, sRGB 0.775059 0.239667 0.303526
  , sRGB 0.780517 0.243327 0.299523, sRGB 0.785929 0.247056 0.295477
  , sRGB 0.791293 0.250856 0.291390, sRGB 0.796607 0.254728 0.287264
  , sRGB 0.801871 0.258674 0.283099, sRGB 0.807082 0.262692 0.278898
  , sRGB 0.812239 0.266786 0.274661, sRGB 0.817341 0.270954 0.270390
  , sRGB 0.822386 0.275197 0.266085, sRGB 0.827372 0.279517 0.261750
  , sRGB 0.832299 0.283913 0.257383, sRGB 0.837165 0.288385 0.252988
  , sRGB 0.841969 0.292933 0.248564, sRGB 0.846709 0.297559 0.244113
  , sRGB 0.851384 0.302260 0.239636, sRGB 0.855992 0.307038 0.235133
  , sRGB 0.860533 0.311892 0.230606, sRGB 0.865006 0.316822 0.226055
  , sRGB 0.869409 0.321827 0.221482, sRGB 0.873741 0.326906 0.216886
  , sRGB 0.878001 0.332060 0.212268, sRGB 0.882188 0.337287 0.207628
  , sRGB 0.886302 0.342586 0.202968, sRGB 0.890341 0.347957 0.198286
  , sRGB 0.894305 0.353399 0.193584, sRGB 0.898192 0.358911 0.188860
  , sRGB 0.902003 0.364492 0.184116, sRGB 0.905735 0.370140 0.179350
  , sRGB 0.909390 0.375856 0.174563, sRGB 0.912966 0.381636 0.169755
  , sRGB 0.916462 0.387481 0.164924, sRGB 0.919879 0.393389 0.160070
  , sRGB 0.923215 0.399359 0.155193, sRGB 0.926470 0.405389 0.150292
  , sRGB 0.929644 0.411479 0.145367, sRGB 0.932737 0.417627 0.140417
  , sRGB 0.935747 0.423831 0.135440, sRGB 0.938675 0.430091 0.130438
  , sRGB 0.941521 0.436405 0.125409, sRGB 0.944285 0.442772 0.120354
  , sRGB 0.946965 0.449191 0.115272, sRGB 0.949562 0.455660 0.110164
  , sRGB 0.952075 0.462178 0.105031, sRGB 0.954506 0.468744 0.099874
  , sRGB 0.956852 0.475356 0.094695, sRGB 0.959114 0.482014 0.089499
  , sRGB 0.961293 0.488716 0.084289, sRGB 0.963387 0.495462 0.079073
  , sRGB 0.965397 0.502249 0.073859, sRGB 0.967322 0.509078 0.068659
  , sRGB 0.969163 0.515946 0.063488, sRGB 0.970919 0.522853 0.058367
  , sRGB 0.972590 0.529798 0.053324, sRGB 0.974176 0.536780 0.048392
  , sRGB 0.975677 0.543798 0.043618, sRGB 0.977092 0.550850 0.039050
  , sRGB 0.978422 0.557937 0.034931, sRGB 0.979666 0.565057 0.031409
  , sRGB 0.980824 0.572209 0.028508, sRGB 0.981895 0.579392 0.026250
  , sRGB 0.982881 0.586606 0.024661, sRGB 0.983779 0.593849 0.023770
  , sRGB 0.984591 0.601122 0.023606, sRGB 0.985315 0.608422 0.024202
  , sRGB 0.985952 0.615750 0.025592, sRGB 0.986502 0.623105 0.027814
  , sRGB 0.986964 0.630485 0.030908, sRGB 0.987337 0.637890 0.034916
  , sRGB 0.987622 0.645320 0.039886, sRGB 0.987819 0.652773 0.045581
  , sRGB 0.987926 0.660250 0.051750, sRGB 0.987945 0.667748 0.058329
  , sRGB 0.987874 0.675267 0.065257, sRGB 0.987714 0.682807 0.072489
  , sRGB 0.987464 0.690366 0.079990, sRGB 0.987124 0.697944 0.087731
  , sRGB 0.986694 0.705540 0.095694, sRGB 0.986175 0.713153 0.103863
  , sRGB 0.985566 0.720782 0.112229, sRGB 0.984865 0.728427 0.120785
  , sRGB 0.984075 0.736087 0.129527, sRGB 0.983196 0.743758 0.138453
  , sRGB 0.982228 0.751442 0.147565, sRGB 0.981173 0.759135 0.156863
  , sRGB 0.980032 0.766837 0.166353, sRGB 0.978806 0.774545 0.176037
  , sRGB 0.977497 0.782258 0.185923, sRGB 0.976108 0.789974 0.196018
  , sRGB 0.974638 0.797692 0.206332, sRGB 0.973088 0.805409 0.216877
  , sRGB 0.971468 0.813122 0.227658, sRGB 0.969783 0.820825 0.238686
  , sRGB 0.968041 0.828515 0.249972, sRGB 0.966243 0.836191 0.261534
  , sRGB 0.964394 0.843848 0.273391, sRGB 0.962517 0.851476 0.285546
  , sRGB 0.960626 0.859069 0.298010, sRGB 0.958720 0.866624 0.310820
  , sRGB 0.956834 0.874129 0.323974, sRGB 0.954997 0.881569 0.337475
  , sRGB 0.953215 0.888942 0.351369, sRGB 0.951546 0.896226 0.365627
  , sRGB 0.950018 0.903409 0.380271, sRGB 0.948683 0.910473 0.395289
  , sRGB 0.947594 0.917399 0.410665, sRGB 0.946809 0.924168 0.426373
  , sRGB 0.946392 0.930761 0.442367, sRGB 0.946403 0.937159 0.458592
  , sRGB 0.946903 0.943348 0.474970, sRGB 0.947937 0.949318 0.491426
  , sRGB 0.949545 0.955063 0.507860, sRGB 0.951740 0.960587 0.524203
  , sRGB 0.954529 0.965896 0.540361, sRGB 0.957896 0.971003 0.556275
  , sRGB 0.961812 0.975924 0.571925, sRGB 0.966249 0.980678 0.587206
  , sRGB 0.971162 0.985282 0.602154, sRGB 0.976511 0.989753 0.616760
  , sRGB 0.982257 0.994109 0.631017, sRGB 0.988362 0.998364 0.644924
  ]

-- | The plasma colour map taken from https://bids.github.io/colormap/.
--
-- <<diagrams/src_Plots_Style_plasmaBar.svg#diagram=plasmaBar&width=600>>
plasma :: ColourMap
plasma = colourMap $ zip [1..]
  [ sRGB 0.050383 0.029803 0.527975, sRGB 0.063536 0.028426 0.533124
  , sRGB 0.075353 0.027206 0.538007, sRGB 0.086222 0.026125 0.542658
  , sRGB 0.096379 0.025165 0.547103, sRGB 0.105980 0.024309 0.551368
  , sRGB 0.115124 0.023556 0.555468, sRGB 0.123903 0.022878 0.559423
  , sRGB 0.132381 0.022258 0.563250, sRGB 0.140603 0.021687 0.566959
  , sRGB 0.148607 0.021154 0.570562, sRGB 0.156421 0.020651 0.574065
  , sRGB 0.164070 0.020171 0.577478, sRGB 0.171574 0.019706 0.580806
  , sRGB 0.178950 0.019252 0.584054, sRGB 0.186213 0.018803 0.587228
  , sRGB 0.193374 0.018354 0.590330, sRGB 0.200445 0.017902 0.593364
  , sRGB 0.207435 0.017442 0.596333, sRGB 0.214350 0.016973 0.599239
  , sRGB 0.221197 0.016497 0.602083, sRGB 0.227983 0.016007 0.604867
  , sRGB 0.234715 0.015502 0.607592, sRGB 0.241396 0.014979 0.610259
  , sRGB 0.248032 0.014439 0.612868, sRGB 0.254627 0.013882 0.615419
  , sRGB 0.261183 0.013308 0.617911, sRGB 0.267703 0.012716 0.620346
  , sRGB 0.274191 0.012109 0.622722, sRGB 0.280648 0.011488 0.625038
  , sRGB 0.287076 0.010855 0.627295, sRGB 0.293478 0.010213 0.629490
  , sRGB 0.299855 0.009561 0.631624, sRGB 0.306210 0.008902 0.633694
  , sRGB 0.312543 0.008239 0.635700, sRGB 0.318856 0.007576 0.637640
  , sRGB 0.325150 0.006915 0.639512, sRGB 0.331426 0.006261 0.641316
  , sRGB 0.337683 0.005618 0.643049, sRGB 0.343925 0.004991 0.644710
  , sRGB 0.350150 0.004382 0.646298, sRGB 0.356359 0.003798 0.647810
  , sRGB 0.362553 0.003243 0.649245, sRGB 0.368733 0.002724 0.650601
  , sRGB 0.374897 0.002245 0.651876, sRGB 0.381047 0.001814 0.653068
  , sRGB 0.387183 0.001434 0.654177, sRGB 0.393304 0.001114 0.655199
  , sRGB 0.399411 0.000859 0.656133, sRGB 0.405503 0.000678 0.656977
  , sRGB 0.411580 0.000577 0.657730, sRGB 0.417642 0.000564 0.658390
  , sRGB 0.423689 0.000646 0.658956, sRGB 0.429719 0.000831 0.659425
  , sRGB 0.435734 0.001127 0.659797, sRGB 0.441732 0.001540 0.660069
  , sRGB 0.447714 0.002080 0.660240, sRGB 0.453677 0.002755 0.660310
  , sRGB 0.459623 0.003574 0.660277, sRGB 0.465550 0.004545 0.660139
  , sRGB 0.471457 0.005678 0.659897, sRGB 0.477344 0.006980 0.659549
  , sRGB 0.483210 0.008460 0.659095, sRGB 0.489055 0.010127 0.658534
  , sRGB 0.494877 0.011990 0.657865, sRGB 0.500678 0.014055 0.657088
  , sRGB 0.506454 0.016333 0.656202, sRGB 0.512206 0.018833 0.655209
  , sRGB 0.517933 0.021563 0.654109, sRGB 0.523633 0.024532 0.652901
  , sRGB 0.529306 0.027747 0.651586, sRGB 0.534952 0.031217 0.650165
  , sRGB 0.540570 0.034950 0.648640, sRGB 0.546157 0.038954 0.647010
  , sRGB 0.551715 0.043136 0.645277, sRGB 0.557243 0.047331 0.643443
  , sRGB 0.562738 0.051545 0.641509, sRGB 0.568201 0.055778 0.639477
  , sRGB 0.573632 0.060028 0.637349, sRGB 0.579029 0.064296 0.635126
  , sRGB 0.584391 0.068579 0.632812, sRGB 0.589719 0.072878 0.630408
  , sRGB 0.595011 0.077190 0.627917, sRGB 0.600266 0.081516 0.625342
  , sRGB 0.605485 0.085854 0.622686, sRGB 0.610667 0.090204 0.619951
  , sRGB 0.615812 0.094564 0.617140, sRGB 0.620919 0.098934 0.614257
  , sRGB 0.625987 0.103312 0.611305, sRGB 0.631017 0.107699 0.608287
  , sRGB 0.636008 0.112092 0.605205, sRGB 0.640959 0.116492 0.602065
  , sRGB 0.645872 0.120898 0.598867, sRGB 0.650746 0.125309 0.595617
  , sRGB 0.655580 0.129725 0.592317, sRGB 0.660374 0.134144 0.588971
  , sRGB 0.665129 0.138566 0.585582, sRGB 0.669845 0.142992 0.582154
  , sRGB 0.674522 0.147419 0.578688, sRGB 0.679160 0.151848 0.575189
  , sRGB 0.683758 0.156278 0.571660, sRGB 0.688318 0.160709 0.568103
  , sRGB 0.692840 0.165141 0.564522, sRGB 0.697324 0.169573 0.560919
  , sRGB 0.701769 0.174005 0.557296, sRGB 0.706178 0.178437 0.553657
  , sRGB 0.710549 0.182868 0.550004, sRGB 0.714883 0.187299 0.546338
  , sRGB 0.719181 0.191729 0.542663, sRGB 0.723444 0.196158 0.538981
  , sRGB 0.727670 0.200586 0.535293, sRGB 0.731862 0.205013 0.531601
  , sRGB 0.736019 0.209439 0.527908, sRGB 0.740143 0.213864 0.524216
  , sRGB 0.744232 0.218288 0.520524, sRGB 0.748289 0.222711 0.516834
  , sRGB 0.752312 0.227133 0.513149, sRGB 0.756304 0.231555 0.509468
  , sRGB 0.760264 0.235976 0.505794, sRGB 0.764193 0.240396 0.502126
  , sRGB 0.768090 0.244817 0.498465, sRGB 0.771958 0.249237 0.494813
  , sRGB 0.775796 0.253658 0.491171, sRGB 0.779604 0.258078 0.487539
  , sRGB 0.783383 0.262500 0.483918, sRGB 0.787133 0.266922 0.480307
  , sRGB 0.790855 0.271345 0.476706, sRGB 0.794549 0.275770 0.473117
  , sRGB 0.798216 0.280197 0.469538, sRGB 0.801855 0.284626 0.465971
  , sRGB 0.805467 0.289057 0.462415, sRGB 0.809052 0.293491 0.458870
  , sRGB 0.812612 0.297928 0.455338, sRGB 0.816144 0.302368 0.451816
  , sRGB 0.819651 0.306812 0.448306, sRGB 0.823132 0.311261 0.444806
  , sRGB 0.826588 0.315714 0.441316, sRGB 0.830018 0.320172 0.437836
  , sRGB 0.833422 0.324635 0.434366, sRGB 0.836801 0.329105 0.430905
  , sRGB 0.840155 0.333580 0.427455, sRGB 0.843484 0.338062 0.424013
  , sRGB 0.846788 0.342551 0.420579, sRGB 0.850066 0.347048 0.417153
  , sRGB 0.853319 0.351553 0.413734, sRGB 0.856547 0.356066 0.410322
  , sRGB 0.859750 0.360588 0.406917, sRGB 0.862927 0.365119 0.403519
  , sRGB 0.866078 0.369660 0.400126, sRGB 0.869203 0.374212 0.396738
  , sRGB 0.872303 0.378774 0.393355, sRGB 0.875376 0.383347 0.389976
  , sRGB 0.878423 0.387932 0.386600, sRGB 0.881443 0.392529 0.383229
  , sRGB 0.884436 0.397139 0.379860, sRGB 0.887402 0.401762 0.376494
  , sRGB 0.890340 0.406398 0.373130, sRGB 0.893250 0.411048 0.369768
  , sRGB 0.896131 0.415712 0.366407, sRGB 0.898984 0.420392 0.363047
  , sRGB 0.901807 0.425087 0.359688, sRGB 0.904601 0.429797 0.356329
  , sRGB 0.907365 0.434524 0.352970, sRGB 0.910098 0.439268 0.349610
  , sRGB 0.912800 0.444029 0.346251, sRGB 0.915471 0.448807 0.342890
  , sRGB 0.918109 0.453603 0.339529, sRGB 0.920714 0.458417 0.336166
  , sRGB 0.923287 0.463251 0.332801, sRGB 0.925825 0.468103 0.329435
  , sRGB 0.928329 0.472975 0.326067, sRGB 0.930798 0.477867 0.322697
  , sRGB 0.933232 0.482780 0.319325, sRGB 0.935630 0.487712 0.315952
  , sRGB 0.937990 0.492667 0.312575, sRGB 0.940313 0.497642 0.309197
  , sRGB 0.942598 0.502639 0.305816, sRGB 0.944844 0.507658 0.302433
  , sRGB 0.947051 0.512699 0.299049, sRGB 0.949217 0.517763 0.295662
  , sRGB 0.951344 0.522850 0.292275, sRGB 0.953428 0.527960 0.288883
  , sRGB 0.955470 0.533093 0.285490, sRGB 0.957469 0.538250 0.282096
  , sRGB 0.959424 0.543431 0.278701, sRGB 0.961336 0.548636 0.275305
  , sRGB 0.963203 0.553865 0.271909, sRGB 0.965024 0.559118 0.268513
  , sRGB 0.966798 0.564396 0.265118, sRGB 0.968526 0.569700 0.261721
  , sRGB 0.970205 0.575028 0.258325, sRGB 0.971835 0.580382 0.254931
  , sRGB 0.973416 0.585761 0.251540, sRGB 0.974947 0.591165 0.248151
  , sRGB 0.976428 0.596595 0.244767, sRGB 0.977856 0.602051 0.241387
  , sRGB 0.979233 0.607532 0.238013, sRGB 0.980556 0.613039 0.234646
  , sRGB 0.981826 0.618572 0.231287, sRGB 0.983041 0.624131 0.227937
  , sRGB 0.984199 0.629718 0.224595, sRGB 0.985301 0.635330 0.221265
  , sRGB 0.986345 0.640969 0.217948, sRGB 0.987332 0.646633 0.214648
  , sRGB 0.988260 0.652325 0.211364, sRGB 0.989128 0.658043 0.208100
  , sRGB 0.989935 0.663787 0.204859, sRGB 0.990681 0.669558 0.201642
  , sRGB 0.991365 0.675355 0.198453, sRGB 0.991985 0.681179 0.195295
  , sRGB 0.992541 0.687030 0.192170, sRGB 0.993032 0.692907 0.189084
  , sRGB 0.993456 0.698810 0.186041, sRGB 0.993814 0.704741 0.183043
  , sRGB 0.994103 0.710698 0.180097, sRGB 0.994324 0.716681 0.177208
  , sRGB 0.994474 0.722691 0.174381, sRGB 0.994553 0.728728 0.171622
  , sRGB 0.994561 0.734791 0.168938, sRGB 0.994495 0.740880 0.166335
  , sRGB 0.994355 0.746995 0.163821, sRGB 0.994141 0.753137 0.161404
  , sRGB 0.993851 0.759304 0.159092, sRGB 0.993482 0.765499 0.156891
  , sRGB 0.993033 0.771720 0.154808, sRGB 0.992505 0.777967 0.152855
  , sRGB 0.991897 0.784239 0.151042, sRGB 0.991209 0.790537 0.149377
  , sRGB 0.990439 0.796859 0.147870, sRGB 0.989587 0.803205 0.146529
  , sRGB 0.988648 0.809579 0.145357, sRGB 0.987621 0.815978 0.144363
  , sRGB 0.986509 0.822401 0.143557, sRGB 0.985314 0.828846 0.142945
  , sRGB 0.984031 0.835315 0.142528, sRGB 0.982653 0.841812 0.142303
  , sRGB 0.981190 0.848329 0.142279, sRGB 0.979644 0.854866 0.142453
  , sRGB 0.977995 0.861432 0.142808, sRGB 0.976265 0.868016 0.143351
  , sRGB 0.974443 0.874622 0.144061, sRGB 0.972530 0.881250 0.144923
  , sRGB 0.970533 0.887896 0.145919, sRGB 0.968443 0.894564 0.147014
  , sRGB 0.966271 0.901249 0.148180, sRGB 0.964021 0.907950 0.149370
  , sRGB 0.961681 0.914672 0.150520, sRGB 0.959276 0.921407 0.151566
  , sRGB 0.956808 0.928152 0.152409, sRGB 0.954287 0.934908 0.152921
  , sRGB 0.951726 0.941671 0.152925, sRGB 0.949151 0.948435 0.152178
  , sRGB 0.946602 0.955190 0.150328, sRGB 0.944152 0.961916 0.146861
  , sRGB 0.941896 0.968590 0.140956, sRGB 0.940015 0.975158 0.131326
  ]

-- | The viridis colour map taken from https://bids.github.io/colormap/.
--   This is the default colour map.
--
-- <<diagrams/src_Plots_Style_viridisBar.svg#diagram=viridisBar&width=600>>
viridis :: ColourMap
viridis = colourMap $ zip [1..]
  [ sRGB 0.267004 0.004874 0.329415, sRGB 0.268510 0.009605 0.335427
  , sRGB 0.269944 0.014625 0.341379, sRGB 0.271305 0.019942 0.347269
  , sRGB 0.272594 0.025563 0.353093, sRGB 0.273809 0.031497 0.358853
  , sRGB 0.274952 0.037752 0.364543, sRGB 0.276022 0.044167 0.370164
  , sRGB 0.277018 0.050344 0.375715, sRGB 0.277941 0.056324 0.381191
  , sRGB 0.278791 0.062145 0.386592, sRGB 0.279566 0.067836 0.391917
  , sRGB 0.280267 0.073417 0.397163, sRGB 0.280894 0.078907 0.402329
  , sRGB 0.281446 0.084320 0.407414, sRGB 0.281924 0.089666 0.412415
  , sRGB 0.282327 0.094955 0.417331, sRGB 0.282656 0.100196 0.422160
  , sRGB 0.282910 0.105393 0.426902, sRGB 0.283091 0.110553 0.431554
  , sRGB 0.283197 0.115680 0.436115, sRGB 0.283229 0.120777 0.440584
  , sRGB 0.283187 0.125848 0.444960, sRGB 0.283072 0.130895 0.449241
  , sRGB 0.282884 0.135920 0.453427, sRGB 0.282623 0.140926 0.457517
  , sRGB 0.282290 0.145912 0.461510, sRGB 0.281887 0.150881 0.465405
  , sRGB 0.281412 0.155834 0.469201, sRGB 0.280868 0.160771 0.472899
  , sRGB 0.280255 0.165693 0.476498, sRGB 0.279574 0.170599 0.479997
  , sRGB 0.278826 0.175490 0.483397, sRGB 0.278012 0.180367 0.486697
  , sRGB 0.277134 0.185228 0.489898, sRGB 0.276194 0.190074 0.493001
  , sRGB 0.275191 0.194905 0.496005, sRGB 0.274128 0.199721 0.498911
  , sRGB 0.273006 0.204520 0.501721, sRGB 0.271828 0.209303 0.504434
  , sRGB 0.270595 0.214069 0.507052, sRGB 0.269308 0.218818 0.509577
  , sRGB 0.267968 0.223549 0.512008, sRGB 0.266580 0.228262 0.514349
  , sRGB 0.265145 0.232956 0.516599, sRGB 0.263663 0.237631 0.518762
  , sRGB 0.262138 0.242286 0.520837, sRGB 0.260571 0.246922 0.522828
  , sRGB 0.258965 0.251537 0.524736, sRGB 0.257322 0.256130 0.526563
  , sRGB 0.255645 0.260703 0.528312, sRGB 0.253935 0.265254 0.529983
  , sRGB 0.252194 0.269783 0.531579, sRGB 0.250425 0.274290 0.533103
  , sRGB 0.248629 0.278775 0.534556, sRGB 0.246811 0.283237 0.535941
  , sRGB 0.244972 0.287675 0.537260, sRGB 0.243113 0.292092 0.538516
  , sRGB 0.241237 0.296485 0.539709, sRGB 0.239346 0.300855 0.540844
  , sRGB 0.237441 0.305202 0.541921, sRGB 0.235526 0.309527 0.542944
  , sRGB 0.233603 0.313828 0.543914, sRGB 0.231674 0.318106 0.544834
  , sRGB 0.229739 0.322361 0.545706, sRGB 0.227802 0.326594 0.546532
  , sRGB 0.225863 0.330805 0.547314, sRGB 0.223925 0.334994 0.548053
  , sRGB 0.221989 0.339161 0.548752, sRGB 0.220057 0.343307 0.549413
  , sRGB 0.218130 0.347432 0.550038, sRGB 0.216210 0.351535 0.550627
  , sRGB 0.214298 0.355619 0.551184, sRGB 0.212395 0.359683 0.551710
  , sRGB 0.210503 0.363727 0.552206, sRGB 0.208623 0.367752 0.552675
  , sRGB 0.206756 0.371758 0.553117, sRGB 0.204903 0.375746 0.553533
  , sRGB 0.203063 0.379716 0.553925, sRGB 0.201239 0.383670 0.554294
  , sRGB 0.199430 0.387607 0.554642, sRGB 0.197636 0.391528 0.554969
  , sRGB 0.195860 0.395433 0.555276, sRGB 0.194100 0.399323 0.555565
  , sRGB 0.192357 0.403199 0.555836, sRGB 0.190631 0.407061 0.556089
  , sRGB 0.188923 0.410910 0.556326, sRGB 0.187231 0.414746 0.556547
  , sRGB 0.185556 0.418570 0.556753, sRGB 0.183898 0.422383 0.556944
  , sRGB 0.182256 0.426184 0.557120, sRGB 0.180629 0.429975 0.557282
  , sRGB 0.179019 0.433756 0.557430, sRGB 0.177423 0.437527 0.557565
  , sRGB 0.175841 0.441290 0.557685, sRGB 0.174274 0.445044 0.557792
  , sRGB 0.172719 0.448791 0.557885, sRGB 0.171176 0.452530 0.557965
  , sRGB 0.169646 0.456262 0.558030, sRGB 0.168126 0.459988 0.558082
  , sRGB 0.166617 0.463708 0.558119, sRGB 0.165117 0.467423 0.558141
  , sRGB 0.163625 0.471133 0.558148, sRGB 0.162142 0.474838 0.558140
  , sRGB 0.160665 0.478540 0.558115, sRGB 0.159194 0.482237 0.558073
  , sRGB 0.157729 0.485932 0.558013, sRGB 0.156270 0.489624 0.557936
  , sRGB 0.154815 0.493313 0.557840, sRGB 0.153364 0.497000 0.557724
  , sRGB 0.151918 0.500685 0.557587, sRGB 0.150476 0.504369 0.557430
  , sRGB 0.149039 0.508051 0.557250, sRGB 0.147607 0.511733 0.557049
  , sRGB 0.146180 0.515413 0.556823, sRGB 0.144759 0.519093 0.556572
  , sRGB 0.143343 0.522773 0.556295, sRGB 0.141935 0.526453 0.555991
  , sRGB 0.140536 0.530132 0.555659, sRGB 0.139147 0.533812 0.555298
  , sRGB 0.137770 0.537492 0.554906, sRGB 0.136408 0.541173 0.554483
  , sRGB 0.135066 0.544853 0.554029, sRGB 0.133743 0.548535 0.553541
  , sRGB 0.132444 0.552216 0.553018, sRGB 0.131172 0.555899 0.552459
  , sRGB 0.129933 0.559582 0.551864, sRGB 0.128729 0.563265 0.551229
  , sRGB 0.127568 0.566949 0.550556, sRGB 0.126453 0.570633 0.549841
  , sRGB 0.125394 0.574318 0.549086, sRGB 0.124395 0.578002 0.548287
  , sRGB 0.123463 0.581687 0.547445, sRGB 0.122606 0.585371 0.546557
  , sRGB 0.121831 0.589055 0.545623, sRGB 0.121148 0.592739 0.544641
  , sRGB 0.120565 0.596422 0.543611, sRGB 0.120092 0.600104 0.542530
  , sRGB 0.119738 0.603785 0.541400, sRGB 0.119512 0.607464 0.540218
  , sRGB 0.119423 0.611141 0.538982, sRGB 0.119483 0.614817 0.537692
  , sRGB 0.119699 0.618490 0.536347, sRGB 0.120081 0.622161 0.534946
  , sRGB 0.120638 0.625828 0.533488, sRGB 0.121380 0.629492 0.531973
  , sRGB 0.122312 0.633153 0.530398, sRGB 0.123444 0.636809 0.528763
  , sRGB 0.124780 0.640461 0.527068, sRGB 0.126326 0.644107 0.525311
  , sRGB 0.128087 0.647749 0.523491, sRGB 0.130067 0.651384 0.521608
  , sRGB 0.132268 0.655014 0.519661, sRGB 0.134692 0.658636 0.517649
  , sRGB 0.137339 0.662252 0.515571, sRGB 0.140210 0.665859 0.513427
  , sRGB 0.143303 0.669459 0.511215, sRGB 0.146616 0.673050 0.508936
  , sRGB 0.150148 0.676631 0.506589, sRGB 0.153894 0.680203 0.504172
  , sRGB 0.157851 0.683765 0.501686, sRGB 0.162016 0.687316 0.499129
  , sRGB 0.166383 0.690856 0.496502, sRGB 0.170948 0.694384 0.493803
  , sRGB 0.175707 0.697900 0.491033, sRGB 0.180653 0.701402 0.488189
  , sRGB 0.185783 0.704891 0.485273, sRGB 0.191090 0.708366 0.482284
  , sRGB 0.196571 0.711827 0.479221, sRGB 0.202219 0.715272 0.476084
  , sRGB 0.208030 0.718701 0.472873, sRGB 0.214000 0.722114 0.469588
  , sRGB 0.220124 0.725509 0.466226, sRGB 0.226397 0.728888 0.462789
  , sRGB 0.232815 0.732247 0.459277, sRGB 0.239374 0.735588 0.455688
  , sRGB 0.246070 0.738910 0.452024, sRGB 0.252899 0.742211 0.448284
  , sRGB 0.259857 0.745492 0.444467, sRGB 0.266941 0.748751 0.440573
  , sRGB 0.274149 0.751988 0.436601, sRGB 0.281477 0.755203 0.432552
  , sRGB 0.288921 0.758394 0.428426, sRGB 0.296479 0.761561 0.424223
  , sRGB 0.304148 0.764704 0.419943, sRGB 0.311925 0.767822 0.415586
  , sRGB 0.319809 0.770914 0.411152, sRGB 0.327796 0.773980 0.406640
  , sRGB 0.335885 0.777018 0.402049, sRGB 0.344074 0.780029 0.397381
  , sRGB 0.352360 0.783011 0.392636, sRGB 0.360741 0.785964 0.387814
  , sRGB 0.369214 0.788888 0.382914, sRGB 0.377779 0.791781 0.377939
  , sRGB 0.386433 0.794644 0.372886, sRGB 0.395174 0.797475 0.367757
  , sRGB 0.404001 0.800275 0.362552, sRGB 0.412913 0.803041 0.357269
  , sRGB 0.421908 0.805774 0.351910, sRGB 0.430983 0.808473 0.346476
  , sRGB 0.440137 0.811138 0.340967, sRGB 0.449368 0.813768 0.335384
  , sRGB 0.458674 0.816363 0.329727, sRGB 0.468053 0.818921 0.323998
  , sRGB 0.477504 0.821444 0.318195, sRGB 0.487026 0.823929 0.312321
  , sRGB 0.496615 0.826376 0.306377, sRGB 0.506271 0.828786 0.300362
  , sRGB 0.515992 0.831158 0.294279, sRGB 0.525776 0.833491 0.288127
  , sRGB 0.535621 0.835785 0.281908, sRGB 0.545524 0.838039 0.275626
  , sRGB 0.555484 0.840254 0.269281, sRGB 0.565498 0.842430 0.262877
  , sRGB 0.575563 0.844566 0.256415, sRGB 0.585678 0.846661 0.249897
  , sRGB 0.595839 0.848717 0.243329, sRGB 0.606045 0.850733 0.236712
  , sRGB 0.616293 0.852709 0.230052, sRGB 0.626579 0.854645 0.223353
  , sRGB 0.636902 0.856542 0.216620, sRGB 0.647257 0.858400 0.209861
  , sRGB 0.657642 0.860219 0.203082, sRGB 0.668054 0.861999 0.196293
  , sRGB 0.678489 0.863742 0.189503, sRGB 0.688944 0.865448 0.182725
  , sRGB 0.699415 0.867117 0.175971, sRGB 0.709898 0.868751 0.169257
  , sRGB 0.720391 0.870350 0.162603, sRGB 0.730889 0.871916 0.156029
  , sRGB 0.741388 0.873449 0.149561, sRGB 0.751884 0.874951 0.143228
  , sRGB 0.762373 0.876424 0.137064, sRGB 0.772852 0.877868 0.131109
  , sRGB 0.783315 0.879285 0.125405, sRGB 0.793760 0.880678 0.120005
  , sRGB 0.804182 0.882046 0.114965, sRGB 0.814576 0.883393 0.110347
  , sRGB 0.824940 0.884720 0.106217, sRGB 0.835270 0.886029 0.102646
  , sRGB 0.845561 0.887322 0.099702, sRGB 0.855810 0.888601 0.097452
  , sRGB 0.866013 0.889868 0.095953, sRGB 0.876168 0.891125 0.095250
  , sRGB 0.886271 0.892374 0.095374, sRGB 0.896320 0.893616 0.096335
  , sRGB 0.906311 0.894855 0.098125, sRGB 0.916242 0.896091 0.100717
  , sRGB 0.926106 0.897330 0.104071, sRGB 0.935904 0.898570 0.108131
  , sRGB 0.945636 0.899815 0.112838, sRGB 0.955300 0.901065 0.118128
  , sRGB 0.964894 0.902323 0.123941, sRGB 0.974417 0.903590 0.130215
  , sRGB 0.983868 0.904867 0.136897, sRGB 0.993248 0.906157 0.143936
  ]

