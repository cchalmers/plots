{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module Plots.Themes where

import Control.Lens     hiding (( # ))
import Data.Colour.SRGB
import Data.List
import Data.Typeable
import Data.Default
import Diagrams.Prelude
import Data.Monoid.Recommend

-- * An entry can be applied to a 'Plot' to change it's style.
data ThemeEntry b = ThemeEntry
  { _themeEntryColor  :: Colour Double
  , _themeLineStyle   :: Style R2
  , _themeMarkerStyle :: Style R2
  , _themeFillStyle   :: Style R2
  , _themeMarker      :: Diagram b R2
  } deriving Typeable

makeClassy ''ThemeEntry

-- class HasThemeEntry a b | a -> b where
--   themeEntry :: Lens' a (Recommend (ThemeEntry b))

type instance V (ThemeEntry b) = R2

instance Renderable (Path R2) b => Default (ThemeEntry b) where
  def = ThemeEntry
          { _themeEntryColor  = black
          , _themeLineStyle   = mempty
          , _themeMarkerStyle = mempty
          , _themeFillStyle   = mempty
          , _themeMarker      = circle 5
          }

-- * A theme can be applied to multiple plots in an axis.
type Theme b = [ThemeEntry b]

-- * Theme construction

-- | Convienient way to construct themes.
data ThemeContructor = ThemeContructor
  { _constructorColours   :: [Colour Double]
  , _constructLineStyle   :: Colour Double -> Style R2
  , _constructMarkerStyle :: Colour Double -> Style R2
  , _constructFillStyle   :: Colour Double -> Style R2
  , _markerPaths          :: [Path R2]
  }

makeLenses ''ThemeContructor

constructTheme :: Renderable (Path R2) b => ThemeContructor -> Theme b
constructTheme tc = zipWith5 ThemeEntry
  colours
  (tc^.constructLineStyle   <$> colours)
  (tc^.constructMarkerStyle <$> colours)
  (tc^.constructFillStyle   <$> colours)
  (stroke                   <$> tc^.markerPaths)
  where colours = tc ^. constructorColours

coolThemeConstructor :: ThemeContructor
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


coolTheme :: Renderable (Path R2) b => Theme b
coolTheme = constructTheme coolThemeConstructor

corperateTheme :: Renderable (Path R2) b => Theme b
corperateTheme = constructTheme $
  coolThemeConstructor
    & constructorColours .~ corperateColours


colourfullColours :: [Colour Double]
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

corperateColours :: [Colour Double]
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

filledMarkers :: [Path R2]
filledMarkers = map (centerXY . pathFromTrail) $ cycle
  [ circle 0.5
  , square 1
  , triangle 1
  , diamond (1 / sqrt 2)
  , pentagon 0.6
  , cross 1
  , plus 1
  , star' 0.8
  ]

lineMarkers :: [Path R2]
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

prong :: Int -> Double -> Path R2
prong n x = mconcat . take n
          . iterate (rotateBy (1/fromIntegral n))
          $ spoke
  where
    spoke = (0 ^& 0) ~~ (0 ^& x)

diamond :: (TrailLike v, Transformable v, V v ~ R2)
        => Double -> v
diamond = rotateBy (1/8) . square

cross :: Double -> Trail R2
cross = rotateBy (1/8) . plus

plus :: Double -> Trail R2
plus x = wrapTrail . glueLine . mconcat . take 4
       . iterate (rotateBy (1/4)) . onLineSegments init
       $ square (x/3)


star' :: Double -> Trail R2
star' x = wrapTrail . glueLine . mconcat . take 5
        . iterate (rotateBy (-1/5)) $ spoke
  where
    spoke = fromOffsets . map r2 $ [(x/6,x/2), (x/6,-x/2)]

-- showcase :: Renderable (Path R2) b => Theme -> Diagram b R2
-- showcase theme
--   = vcat' (with & sep .~ 0.4) . take 8
--   $ zipWith (\c m -> styleF c m <> lineBehind c) cs ms
--   where
--     lineBehind c = (p2 (-1,0) ~~ p2 (1,0)) # lc c # lwG 0.2

