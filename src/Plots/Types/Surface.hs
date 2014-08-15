{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverlappingInstances   #-}
{-# LANGUAGE ViewPatterns           #-}
{-# LANGUAGE TypeOperators          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Plots.Types.Surface
  ( SurfaceType (..)
  , SurfacePlot
  , mkSurfacePlot
  ) where

import Control.Lens     hiding (transform, ( # ), lmap)
import Diagrams.LinearMap
import Data.Default
import Data.Typeable
import Diagrams.Prelude hiding (view)
import Diagrams.Extra
import Data.Foldable
import Diagrams.ThreeD.Types
import Linear (V2 (..))
-- import Diagrams.Coordinates.Traversals
-- import Diagrams.Coordinates.Isomorphic

-- import Plots.Themes
import Plots.Types

import qualified Data.Vector as V
import Data.Vector (Vector, (!))

data SurfaceType = Mesh
                 | Faceted
                 | Flat

data SurfacePlot b = SurfacePlot
  { _surfaceFunction :: Double -> Double -> Double
  , _surfaceGenericPlot :: GenericPlot b R3
  } deriving Typeable

makeLenses ''SurfacePlot

type instance V (SurfacePlot b) = R3

instance (Renderable (Path R2) b) => Default (SurfacePlot b) where
  def = SurfacePlot
          { _surfaceFunction    = \_ _ -> 0
          , _surfaceGenericPlot = def
          }

instance HasGenericPlot (SurfacePlot b) b where
  genericPlot = surfaceGenericPlot

-- could probably do somehting fancy with zippers but keep it simple for now.
mkSquares :: Vector (Vector P3) -> [(P3, Path R3)]
mkSquares v = do
  let i = V.length v
      j = V.length $ V.head v

  x <- [0 .. i-2]
  y <- [0 .. j-2]

  let ps = [ v ! x     ! y
           , v ! (x+1) ! y
           , v ! (x+1) ! (y+1)
           , v ! x     ! (y+1) ]

  pure (centroid ps, closePath $ pathFromVertices ps)

closePath :: Path v -> Path v
closePath = over (_Wrapped' . mapped . located) closeTrail

calcPoints :: (Double -> Double -> Double) -> Int -> V2 (Double, Double) -> Vector (Vector P3)
calcPoints f n (V2 (xa,xb) (ya,yb)) = V.fromList $ map ylines ys
  where
    ylines y = V.fromList [ mkP3 x y (f x y) | x <- xs ]
    --
    xs = [xa, xa + (xb - xa) / fromIntegral n .. xb]
    ys = [ya, ya + (xb - xa) / fromIntegral n .. yb]

drawSquare :: Renderable (Path R2) b
    => T3 -> (R3 :-* R2) -> T2 -> (P3, Path R3) -> Diagram b R2
drawSquare t3 l t2 (view _z -> z, sq)
  = sq # transform t3
       # lmap l
       # transform t2
       # stroke
       # fc (blend z grey red)

instance (Typeable b, Renderable (Path R2) b) => Plotable (SurfacePlot b) b where
  plot _ t3 l t2 sp = foldMap (drawSquare t3 l t2) sqs
    where sqs = mkSquares $ calcPoints f 20 bs
          f   = sp ^. surfaceFunction
          bs  = V2 (0,5) (0,5)

mkSurfacePlot :: Renderable (Path R2) b
  => (Double -> Double -> Double) -> SurfacePlot b
mkSurfacePlot f = def & surfaceFunction .~ f

