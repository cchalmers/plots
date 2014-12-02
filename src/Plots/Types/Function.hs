{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverlappingInstances   #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Plots.Types.Function
  ( FunctionPlotOptions
  , functionPlotSmooth
  , functionPlotNumPoints
  , functionPlotDiscontinuous
  -- , mkFunctionPlot
    -- * Prism
  -- , _FunctionPlot
  -- , mkFunctionPlot


    -- * Lenses
  -- , functionPlotFunction
  -- , functionPlotNumPoints
  -- , functionPlotSmooth
  -- , functionPlotDiscontinuous

  , ParametricPlot (..)
  , mkParametricPlot
  -- mesh
  -- , MeshPlot (..)
  -- , mkMeshPlot

   -- * Vector field
  -- , VectorField
  -- , mkVectorField
  ) where

import           Control.Lens                    hiding (lmap, transform, ( # ))
import           Data.Default
-- import           Data.Foldable
import           Data.Typeable
-- import           Diagrams.BoundingBox
import           Diagrams.Coordinates.Isomorphic
import           Diagrams.Extra
-- import           Diagrams.LinearMap
import           Diagrams.Prelude                hiding (view)

-- import           Linear.V3
import           Plots.Themes
import           Plots.Types
-- import           Data.Traversable  as T
-- import           Plots.Utils

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

------------------------------------------------------------------------
-- Parametric plot
------------------------------------------------------------------------

data ParametricPlot v n = ParametricPlot
  { _parametricFunction    :: n -> Point v n
  , _parametricDomain      :: (n, n)
  , _parametricPlotOptions :: FunctionPlotOptions n
  } deriving Typeable

makeLenses ''ParametricPlot

type instance V (ParametricPlot v n) = v
type instance N (ParametricPlot v n) = n

instance HasFunctionPlotOptions (ParametricPlot v n) n where
  functionPlotOptions = parametricPlotOptions

instance (Typeable b, TypeableFloat n, Enum n, Renderable (Path V2 n) b)
    => Plotable (ParametricPlot V2 n) b where
  renderPlotable pp _ t fp = pathFromVertices p
                            # transform t
                            # stroke
                            # applyStyle (pp ^. themeLineStyle)
    where
      p = map f [a, a + 1 / (fp ^. functionPlotNumPoints . to fromIntegral) .. b]
      f = fp ^. parametricFunction
      a = fp ^. parametricDomain . _1
      b = fp ^. parametricDomain . _2

mkParametricPlot :: (PointLike v n p, Additive v, TypeableFloat n) => (n -> p) -> ParametricPlot v n
mkParametricPlot f
  = ParametricPlot
      { _parametricFunction = view unpointLike . f
      , _parametricDomain   = (0,5)
      , _parametricPlotOptions = def
      }

------------------------------------------------------------------------
-- Mesh plot
------------------------------------------------------------------------

data MeshPlot n = MeshPlot
  { _meshFunction    :: n -> n -> n -- ^ $\x y -> z$
  , _meshDomain      :: V2 (n, n)
  -- , _meshPlotOptions :: FunctionPlotOptions b R3
  } deriving Typeable

type instance V (MeshPlot n) = V3
type instance N (MeshPlot n) = n

-- makeLenses ''MeshPlot

instance (Num n) => Default (MeshPlot n) where
  def = MeshPlot
          { _meshFunction    = \_ _ -> 0
          , _meshDomain      = V2 (0,5) (0,5)
          -- , _meshPlotOptions = def & functionPlotNumPoints .~ 10
          }

-- instance HasGenericPlot (MeshPlot b n) where
--   genericPlot = meshPlotGeneric

-- instance HasFunctionPlotOptions (MeshPlot b) b R3 where
--   functionPlotOptions = meshPlotOptions

-- instance (Typeable b, TypeableFloat n, Enum n, Renderable (Path V2 n) b)
--     => Plotable (MeshPlot n) b where
--   renderPlotable pp _ tv l t2 mp =
--     path # transform t
--          # stroke
--          # applyStyle (pp ^. themeLineStyle)
--     where
--       path = foldMap xlines xs <> foldMap ylines ys
--       --
--       f = mp ^. meshFunction
--       xlines x = pathFromVertices [ mkP3 x y (f x y) | y <- ys ]
--       ylines y = pathFromVertices [ mkP3 x y (f x y) | x <- xs ]
--       --
--       xs = [xa, xa + (xb - xa) / n .. xb]
--       ys = [ya, ya + (xb - xa) / n .. yb]
--       (xa,xb) = mp ^. meshDomain . el ex
--       (ya,yb) = mp ^. meshDomain . el ey
--       -- n = mp ^. functionPlotNumPoints . to fromIntegral
--       n = 15

-- mkMeshPlot :: (TypeableFloat n) => (n -> n -> n) -> MeshPlot n
-- mkMeshPlot f = def & meshFunction .~ f

-- ------------------------------------------------------------------------
-- -- Mesh plot
-- ------------------------------------------------------------------------

-- data FunctionPlot n = FunctionPlot
--   { _functionPlotFunction :: n -> n
--   , _functionPlotOpts     :: FunctionPlotOptions n
--   , _functionDomain       :: (n,n)
--   } deriving Typeable

-- makeLenses ''FunctionPlot

-- instance HasFunctionPlotOptions (FunctionPlot n) n where
--   functionPlotOptions = functionPlotOpts

-- type instance V (FunctionPlot n) = V2
-- type instance N (FunctionPlot n) = n

-- -- do I really need a default instance for this?
-- instance (TypeableFloat n) => Default (FunctionPlot n) where
--   def = FunctionPlot { _functionPlotFunction = const 0
--                      , _functionDomain       = (0,5)
--                      , _functionPlotOpts     = def
--                      }

-- -- pathFromVertices :: [Point v n] -> Path v n
-- -- pathFromVertices = fromVertices



-- -- drawFunctionPlot :: Renderable (Path V2 n) b => T2 n -> FunctionPlot b V2 n -> Diagram b V2 n
-- -- drawFunctionPlot t fp = fromVertices functionPath
-- --                           # transform t
-- --                           # applyStyle (fp ^. themeLineStyle)
-- --   where
-- --     functionPath = map f [a, a + 1 / (fp ^. functionPlotNumPoints . to fromIntegral) .. b]
-- --     f            = fp ^. functionPlotFunction
-- --     a            = fp ^. plotBounds . el ex . lowerBound . recommend
-- --     b            = fp ^. plotBounds . el ex . upperBound . recommend

-- instance (Typeable b, TypeableFloat n, Enum n, Renderable (Path V2 n) b) => Plotable (FunctionPlot n) b where
--   renderPlotable pp _ tv l t2 fp =
--     path # transform tv
--          # lmap l
--          # transform t2
--          # stroke
--          # applyStyle (pp ^. themeLineStyle)
--     where
--       path = pathFromVertices $ map (mkP2 <*> f) [a, a + 1 / (fp ^. functionPlotNumPoints . to fromIntegral) .. b]
--       --
--       f = fp ^. functionPlotFunction
--       (a,b) = fp ^. functionDomain -- getBound ex fp


-- -- _FunctionPlot :: Plotable (FunctionPlot n) b => Prism' (Plot b V2 n) (FunctionPlot n)
-- -- _FunctionPlot = _Plot

-- mkFunctionPlot :: (TypeableFloat n) => (n,n) -> (n -> n) -> FunctionPlot n
-- mkFunctionPlot d f = def & functionPlotFunction .~ f
--                          & functionDomain       .~ d


-- ------------------------------------------------------------------------
-- -- Vector field
-- ------------------------------------------------------------------------

-- data VectorField v n = VectorField
--   { _fieldGrad         :: v n -> v n
--   , _fieldPoints       :: BoundingBox v n -> [Point v n]
--   , _vectorFieldArrows :: ArrowOpts n
--   }

-- -- makeLenses ''VectorField

-- mkVectorField :: (VectorLike v n vn, TypeableFloat n) => (vn -> vn) -> VectorField v n
-- mkVectorField f
--   = VectorField
--       { _fieldGrad   = over vectorLike f
--       , _fieldPoints = splitBoundingBox (10 <$ (zero :: Additive v => v Int))
--       , _vectorFieldArrows = def
--       }

-- splitBoundingBox :: (Additive v, Traversable v, Fractional n)
--   => v Int -> BoundingBox v n -> [Point v n]
-- splitBoundingBox xs = maybe [] mkPoints . getCorners
--   where
--     mkPoints (l,u) = T.sequence (liftI3 enumFromToN l u (P xs))

-- (<~>) :: Additive v => v (a -> b) -> v a -> v b
-- (<~>) = liftI2 ($)
-- {-# INLINE (<~>) #-}

-- infixl 4 <~>

-- liftI3 :: Additive f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
-- liftI3 f a b c = liftI2 f a b <~> c
-- {-# INLINE liftI3 #-}

