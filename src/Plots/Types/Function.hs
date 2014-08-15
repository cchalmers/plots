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
import Linear (V2 (..), el, ex, ey)
import Diagrams.Coordinates.Traversals
import Diagrams.Coordinates.Isomorphic

import Plots.Themes
import Plots.Types

-- Options

data FunctionPlotOptions = FunctionPlot
  { _functionPlotNumPoints     :: Int
  , _functionPlotSmooth        :: Bool
  , _functionPlotDiscontinuous :: Maybe Double
  } deriving Typeable

makeClassy ''FunctionPlotOptions

instance Default FunctionPlotOptions where
  def = FunctionPlot
          { _functionPlotNumPoints     = 100
          , _functionPlotSmooth        = False
          , _functionPlotDiscontinuous = Nothing
          }

-- Parametric plots

data ParametricPlot b v = ParametricPlot
  { _parametricFunction    :: Double -> Point v
  , _parametricDomain      :: (Double, Double)
  , _parametricPlotOptions :: FunctionPlotOptions
  , _parametricGenericPlot :: GenericPlot b v
  } deriving Typeable

makeLenses ''ParametricPlot

type instance V (ParametricPlot b v) = v

instance HasFunctionPlotOptions (ParametricPlot b v) where
  functionPlotOptions = parametricPlotOptions

instance HasGenericPlot (ParametricPlot b v) b where
  genericPlot = parametricGenericPlot

instance (Renderable (Path R2) b, HasLinearMap v, Applicative (T v), AdditiveGroup v)
    => Default (ParametricPlot b v) where
  def = ParametricPlot
          { _parametricFunction    = const origin
          , _parametricDomain      = (0,5)
          , _parametricPlotOptions = def
          , _parametricGenericPlot = def
          }

instance (Typeable b, Typeable v, Scalar v ~ Double, Renderable (Path R2) b,
          HasLinearMap v, InnerSpace v)
    => Plotable (ParametricPlot b v) b where
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

mkParametricPlot :: (PointLike a v, Renderable (Path R2) b, Applicative (T v)) => (Double -> a) -> ParametricPlot b v
mkParametricPlot f =
  def & parametricFunction .~ fmap (view diaPoint) f

-- Mesh plot

data MeshPlot b = MeshPlot
  { _meshFunction    :: Double -> Double -> Double
  , _meshDomain      :: V2 (Double, Double)
  -- , _meshPlotOptions :: FunctionPlotOptions b R3
  , _meshPlotGeneric :: GenericPlot b R3
  } deriving Typeable

type instance V (MeshPlot b) = R3

makeLenses ''MeshPlot

instance Renderable (Path R2) b => Default (MeshPlot b) where
  def = MeshPlot
          { _meshFunction    = \_ _ -> 0
          , _meshDomain      = V2 (0,5) (0,5)
          -- , _meshPlotOptions = def & functionPlotNumPoints .~ 10
          , _meshPlotGeneric = def
          }

instance HasGenericPlot (MeshPlot b) b where
  genericPlot = meshPlotGeneric

-- instance HasFunctionPlotOptions (MeshPlot b) b R3 where
--   functionPlotOptions = meshPlotOptions

instance (Typeable b, Renderable (Path R2) b)
    => Plotable (MeshPlot b) b where
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

mkMeshPlot :: Renderable (Path R2) b => (Double -> Double -> Double) -> MeshPlot b
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
