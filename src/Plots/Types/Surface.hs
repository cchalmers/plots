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
-- import Diagrams.Coordinates.Traversals
-- import Diagrams.Coordinates.Isomorphic

-- import Plots.Themes
import Plots.Types

import qualified Data.Vector as V
import Data.Vector (Vector, (!))

type T3 = Transformation V3

data SurfaceType = Mesh
                 | Faceted
                 | Flat

data SurfacePlot b n = SurfacePlot
  { _surfaceFunction :: n -> n -> n
  , _surfaceGenericPlot :: GenericPlot b V3 n
  } deriving Typeable

makeLenses ''SurfacePlot

type instance V (SurfacePlot b n) = V3
type instance N (SurfacePlot b n) = n

instance (TypeableFloat n, Renderable (Path V2 n) b) => Default (SurfacePlot b n) where
  def = SurfacePlot
          { _surfaceFunction    = \_ _ -> 0
          , _surfaceGenericPlot = def
          }

instance HasGenericPlot (SurfacePlot b n) b where
  genericPlot = surfaceGenericPlot

-- could probably do something fancy with zippers but keep it simple for now.
mkSquares :: OrderedField n => Vector (Vector (P3 n)) -> [(P3 n, Path V3 n)]
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

closePath :: Path v n -> Path v n
closePath = over (_Wrapped' . mapped . located) closeTrail

calcPoints :: (Fractional n, Enum n) => (n -> n -> n) -> Int -> V2 (n, n) -> Vector (Vector (P3 n))
calcPoints f n (V2 (xa,xb) (ya,yb)) = V.fromList $ map ylines ys
  where
    ylines y = V.fromList [ mkP3 x y (f x y) | x <- xs ]
    --
    xs = [xa, xa + (xb - xa) / fromIntegral n .. xb]
    ys = [ya, ya + (xb - xa) / fromIntegral n .. yb]

drawSquare :: (TypeableFloat n, Renderable (Path V2 n) b)
    => T3 n -> (V3 n -> V2 n) -> T2 n -> (P3 n, Path V3 n) -> Diagram b V2 n
drawSquare t3 l t2 (fromRational . toRational . view _z -> z, sq)
  = sq # transform t3
       # lmap l
       # transform t2
       # stroke
       # fc (blend z grey red)

instance (TypeableFloat n, Enum n, Typeable b, Renderable (Path V2 n) b) => Plotable (SurfacePlot b n) b where
  plot _ t3 l t2 sp = foldMap (drawSquare t3 l t2) sqs
                        # lineJoin LineJoinBevel
    where sqs = mkSquares $ calcPoints f 20 bs
          f   = sp ^. surfaceFunction
          bs  = V2 (0,5) (0,5)

mkSurfacePlot :: (TypeableFloat n, Renderable (Path V2 n) b)
  => (n -> n -> n) -> SurfacePlot b n
mkSurfacePlot f = def & surfaceFunction .~ f

