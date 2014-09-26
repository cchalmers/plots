{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverlappingInstances   #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Plots.Types.Function
  ( FunctionPlotOptions
  , functionPlotSmooth
  , functionPlotNumPoints
  , functionPlotDiscontinuous
  -- , mkFunctionPlot
    -- * Prism
  , _FunctionPlot
  , mkFunctionPlot

  
    -- * Lenses
  -- , functionPlotFunction
  -- , functionPlotNumPoints
  -- , functionPlotSmooth
  -- , functionPlotDiscontinuous

  , ParametricPlot (..)
  , mkParametricPlot
  -- mesh
  , MeshPlot (..)
  , mkMeshPlot
  ) where

import Control.Lens     hiding (transform, ( # ), lmap)
import Diagrams.LinearMap
import Data.Default
import Data.Typeable
import Diagrams.BoundingBox
import Diagrams.Prelude hiding (view)
import Diagrams.Extra
import Data.Foldable
import Diagrams.Coordinates.Isomorphic

import Plots.Themes
import Plots.Types
import Linear.V3

-- Options

data FunctionPlotOptions n = FunctionPlotOpts
  { _functionPlotNumPoints     :: Int
  , _functionPlotSmooth        :: Bool
  , _functionPlotDiscontinuous :: Maybe n
  } deriving Typeable

makeClassy ''FunctionPlotOptions

instance Default (FunctionPlotOptions n) where
  def = FunctionPlotOpts
          { _functionPlotNumPoints     = 100
          , _functionPlotSmooth        = False
          , _functionPlotDiscontinuous = Nothing
          }

-- Parametric plots

data ParametricPlot b v n = ParametricPlot
  { _parametricFunction    :: n -> Point v n
  , _parametricDomain      :: (n, n)
  , _parametricPlotOptions :: FunctionPlotOptions n
  , _parametricGenericPlot :: GenericPlot b v n
  } deriving Typeable

makeLenses ''ParametricPlot

type instance V (ParametricPlot b v n) = v
type instance N (ParametricPlot b v n) = n

instance HasFunctionPlotOptions (ParametricPlot b v n) n where
  functionPlotOptions = parametricPlotOptions

instance HasGenericPlot (ParametricPlot b v n) b where
  genericPlot = parametricGenericPlot

instance (TypeableFloat n, Renderable (Path V2 n) b, HasLinearMap v)
    => Default (ParametricPlot b v n) where
  def = ParametricPlot
          { _parametricFunction    = const origin
          , _parametricDomain      = (0,5)
          , _parametricPlotOptions = def
          , _parametricGenericPlot = def
          }

instance (Typeable b, Typeable v, TypeableFloat n, Enum n, Renderable (Path V2 n) b,
          HasLinearMap v, Metric v)
    => Plotable (ParametricPlot b v n) b where
  plot _ tv l t2 fp = pathFromVertices p
                            # transform tv
                            # lmap l
                            # transform t2
                            # stroke
                            # applyStyle (fp ^. themeLineStyle)
    where
      p = map f [a, a + 1 / (fp ^. functionPlotNumPoints . to fromIntegral) .. b]
      f = fp ^. parametricFunction
      a = fp ^. parametricDomain  . _1
      b = fp ^. parametricDomain  . _2

mkParametricPlot :: (PointLike v n a, Renderable (Path V2 n) b, TypeableFloat n) => (n -> a) -> ParametricPlot b v n
mkParametricPlot f =
  def & parametricFunction .~ fmap (review pointLike) f

-- Mesh plot

data MeshPlot b n = MeshPlot
  { _meshFunction    :: n -> n -> n -- ^ $\x y -> z$
  , _meshDomain      :: V2 (n, n)
  -- , _meshPlotOptions :: FunctionPlotOptions b R3
  , _meshPlotGeneric :: GenericPlot b V3 n
  } deriving Typeable

type instance V (MeshPlot b n) = V3
type instance N (MeshPlot b n) = n

makeLenses ''MeshPlot

instance (TypeableFloat n, Renderable (Path V2 n) b) => Default (MeshPlot b n) where
  def = MeshPlot
          { _meshFunction    = \_ _ -> 0
          , _meshDomain      = V2 (0,5) (0,5)
          -- , _meshPlotOptions = def & functionPlotNumPoints .~ 10
          , _meshPlotGeneric = def
          }

instance HasGenericPlot (MeshPlot b n) b where
  genericPlot = meshPlotGeneric

-- instance HasFunctionPlotOptions (MeshPlot b) b R3 where
--   functionPlotOptions = meshPlotOptions

instance (Typeable b, TypeableFloat n, Enum n, Renderable (Path V2 n) b)
    => Plotable (MeshPlot b n) b where
  plot _ tv l t2 mp =
    path # transform tv
         # lmap l
         # transform t2
         # stroke
         # applyStyle (mp ^. themeLineStyle) 
    where
      path = foldMap xlines xs <> foldMap ylines ys
      --
      f = mp ^. meshFunction
      xlines x = pathFromVertices [ mkP3 x y (f x y) | y <- ys ]
      ylines y = pathFromVertices [ mkP3 x y (f x y) | x <- xs ]
      --
      xs = [xa, xa + (xb - xa) / n .. xb]
      ys = [ya, ya + (xb - xa) / n .. yb]
      (xa,xb) = mp ^. meshDomain . el ex
      (ya,yb) = mp ^. meshDomain . el ey
      -- n = mp ^. functionPlotNumPoints . to fromIntegral
      n = 15

mkMeshPlot :: (TypeableFloat n, Renderable (Path V2 n) b) => (n -> n -> n) -> MeshPlot b n
mkMeshPlot f = 
  def & meshFunction .~ f

data FunctionPlot b n = FunctionPlot
  { _functionPlotFunction :: n -> n
  , _functionPlotGeneric  :: GenericPlot b V2 n
  , _functionPlotOpts     :: FunctionPlotOptions n
  , _functionDomain       :: (n,n)
  } deriving Typeable

makeLenses ''FunctionPlot

instance HasFunctionPlotOptions (FunctionPlot b n) n where
  functionPlotOptions = functionPlotOpts


type instance V (FunctionPlot b n) = V2
type instance N (FunctionPlot b n) = n

instance HasGenericPlot (FunctionPlot b n) b where
  genericPlot = functionPlotGeneric

-- do I really need a default instance for this?
instance (TypeableFloat n, Renderable (Path V2 n) b) => Default (FunctionPlot b n) where
  def = FunctionPlot { _functionPlotFunction = const 0
                     , _functionDomain       = (0,5)
                     , _functionPlotGeneric  = def
                     , _functionPlotOpts     = def
                     }

-- pathFromVertices :: [Point v n] -> Path v n
-- pathFromVertices = fromVertices



-- drawFunctionPlot :: Renderable (Path V2 n) b => T2 n -> FunctionPlot b V2 n -> Diagram b V2 n
-- drawFunctionPlot t fp = fromVertices functionPath
--                           # transform t
--                           # applyStyle (fp ^. themeLineStyle)
--   where
--     functionPath = map f [a, a + 1 / (fp ^. functionPlotNumPoints . to fromIntegral) .. b]
--     f            = fp ^. functionPlotFunction
--     a            = fp ^. plotBounds . el ex . lowerBound . recommend
--     b            = fp ^. plotBounds . el ex . upperBound . recommend

instance (Typeable b, TypeableFloat n, Enum n, Renderable (Path V2 n) b) => Plotable (FunctionPlot b n) b where
  plot _ tv l t2 fp =
    path # transform tv
         # lmap l
         # transform t2
         # stroke
         # applyStyle (fp ^. themeLineStyle) 
    where
      path = pathFromVertices $ map (mkP2 <*> f) [a, a + 1 / (fp ^. functionPlotNumPoints . to fromIntegral) .. b]
      --
      f = fp ^. functionPlotFunction
      (a,b) = fp ^. functionDomain -- getBound ex fp


_FunctionPlot :: Plotable (FunctionPlot b n) b => Prism' (Plot b V2 n) (FunctionPlot b n)
_FunctionPlot = _Plot

mkFunctionPlot :: (Typeable b, Enum n, TypeableFloat n, Renderable (Path V2 n) b) => (n,n) -> (n -> n) -> FunctionPlot b n
mkFunctionPlot d@(a,b) f = def & functionPlotFunction .~ f
                         & functionDomain       .~ d
                         & plotBoundingBox      .~ fromPoints (map (mkP2 <*> f) [a, a + (b - a) / 20 .. b])


