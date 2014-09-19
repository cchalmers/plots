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
  -- , mkFunctionPlot
    -- * Prism
  -- , _FunctionPlot

  
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
import Diagrams.Prelude hiding (view)
import Diagrams.Extra
import Data.Foldable
import Diagrams.ThreeD.Types
import Diagrams.Coordinates.Isomorphic

import Plots.Themes
import Plots.Types
import Linear.V3

-- Options

data FunctionPlotOptions n = FunctionPlot
  { _functionPlotNumPoints     :: Int
  , _functionPlotSmooth        :: Bool
  , _functionPlotDiscontinuous :: Maybe n
  } deriving Typeable

makeClassy ''FunctionPlotOptions

instance Default (FunctionPlotOptions n) where
  def = FunctionPlot
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




-- drawFunctionPlot :: Renderable (Path R2) b => T2 -> FunctionPlot b R2 -> Diagram b R2
-- drawFunctionPlot t fp = fromVertices functionPath
--                           # transform t
--                           # applyStyle (fp ^. themeLineStyle)
--   where
--     functionPath = map f [a, a + 1 / (fp ^. functionPlotNumPoints . to fromIntegral) .. b]
--     f            = fp ^. functionPlotFunction
--     a            = fp ^. plotBounds . el ex . lowerBound . recommend
--     b            = fp ^. plotBounds . el ex . upperBound . recommend
-- 
-- instance (Typeable b, Renderable (Path R2) b) => Plotable (FunctionPlot b R2) b where
--   plot _ _ _ = drawFunctionPlot
-- 
-- _FunctionPlot :: Plotable (FunctionPlot b R2) b => Prism' (Plot b R2) (FunctionPlot b R2)
-- _FunctionPlot = _Plot
-- 
-- mkFunctionPlot :: (Typeable b, Renderable (Path R2) b) => (Double -> Double) -> Plot b R2
-- mkFunctionPlot f = review _FunctionPlot $
--   def & functionPlotFunction .~ f'
--   where
--     f' x = x ^& f x
-- 
-- 
