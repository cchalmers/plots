{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE FunctionalDependencies    #-}

{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE UndecidableInstances      #-}

{-# LANGUAGE StandaloneDeriving        #-}

module Plots.Types.Function
  ( -- * Function plot options
    FunctionPlotOptions
  , functionPlotNumPoints

  -- , functionPlotSmooth
  -- , functionPlotDiscontinuous
  -- , mkFunctionPlot
  -- , _FunctionPlot
  -- , mkFunctionPlot

  -- , functionPlotFunction
  -- , functionPlotNumPoints
  -- , functionPlotSmooth
  -- , functionPlotDiscontinuous

  -- , MeshPlot (..)
  -- , mkMeshPlot

  -- * Parametric plot
  , ParametricPlot (..)
  , mkParametricPlot
  , parametricDomain
  , mkParametricRangePlot

  -- * Create line
  ,createABLine
  ,createHLine
  ,createVLine

  -- * Vector plot
  , VectorPlot (..)
  , mkVectorPlot
  , mkVectorPointPlot

  -- * Vector plot lenses
  , setArrowOpts

 -- * Parametric plot
  , parametricPlot
  , parametricRangePlot
  , parametricPlot'
  , parametricRangePlot'
  -- , parametricPlotL
  -- , parametricRangePlotL

    -- * Line functions
  , abLinePlot
  , hLinePlot
  , vLinePlot

    -- * Vectors
  , vectorPlot
  , vectorPointPlot
  , vectorPointPlot'
  , vectorPointPlot''
  -- , vectorPointPlotL
  , vectorFieldPlot

  -- , meshPlot
  -- , surfacePlot

  ) where

import           Control.Lens                    hiding (lmap, transform, ( # ))
import           Control.Monad.State.Lazy

import           Data.Default
import           Data.Typeable
import qualified Data.Foldable as F

import           Diagrams.Coordinates.Isomorphic
import           Diagrams.Prelude                hiding (view)

import           Plots.Style
import           Plots.Types
import           Plots.Axis

-- import           Data.Traversable  as T
-- import           Data.Foldable
-- import           Diagrams.BoundingBox
-- import           Diagrams.LinearMap
-- import           Linear.V3
-- import           Plots.Util

------------------------------------------------------------------------
-- Function plot options
------------------------------------------------------------------------

data FunctionPlotOptions n = FunctionPlotOpts
  { _functionPlotNumPoints     :: Int
  } deriving Typeable

--  , _functionPlotSmooth        :: Bool
--  , _functionPlotDiscontinuous :: Maybe n

makeClassy ''FunctionPlotOptions

instance Default (FunctionPlotOptions n) where
  def = FunctionPlotOpts
          { _functionPlotNumPoints     = 100
          }

--          , _functionPlotSmooth        = False
--          , _functionPlotDiscontinuous = Nothing

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

instance (Metric v, OrderedField n, Enum n) => Enveloped (ParametricPlot v n) where
  getEnvelope pa = getEnvelope p
                   where
                     p = map f [a, a + 1 / (pa ^. functionPlotNumPoints . to fromIntegral) .. b]
                     f = pa ^. parametricFunction
                     a = pa ^. parametricDomain . _1
                     b = pa ^. parametricDomain . _2

instance (TypeableFloat n, Enum n, Renderable (Path V2 n) b)
    => Plotable (ParametricPlot V2 n) b where
  renderPlotable s sty pa =
    pathFromVertices p
      # transform (s^.specTrans)
      # stroke
      # applyLineStyle sty
    where
      p = map f [a, a + 1 / (pa ^. functionPlotNumPoints . to fromIntegral) .. b]
      f = pa ^. parametricFunction
      a = pa ^. parametricDomain . _1
      b = pa ^. parametricDomain . _2

  defLegendPic sty ParametricPlot {..}
      = (p2 (-10,0) ~~ p2 (10,0))
          # applyLineStyle sty

pathFromVertices :: (Metric v, OrderedField n, Fractional (v n)) => [Point v n] -> Path v n
pathFromVertices = cubicSpline False

-- | Create a parametric plot given a function on range (0,5).
mkParametricPlot :: (PointLike v n p, TypeableFloat n) => (n -> p) -> ParametricPlot v n
mkParametricPlot f
  = ParametricPlot
      { _parametricFunction = view unpointLike . f
      , _parametricDomain   = (0,5)
      , _parametricPlotOptions = def
      }

-- | Create a parametric plot given a function and a range.
mkParametricRangePlot :: PointLike v n p => (n -> p) -> (n, n)-> ParametricPlot v n
mkParametricRangePlot f d
  = ParametricPlot
      { _parametricFunction = view unpointLike . f
      , _parametricDomain   = d
      , _parametricPlotOptions = def
      }

-- | Functions to create ab, vertical and horizontal lines.

createABLine :: Num n => n -> n -> n -> P2 n
createABLine slope intercept x = p2 (x ,(slope*x) +  intercept)

createHLine :: n -> n -> P2 n
createHLine intercept x = p2 (x, intercept)

createVLine :: n -> n -> P2 n
createVLine intercept x = p2 (intercept, x)

------------------------------------------------------------------------
-- Vector Plot
------------------------------------------------------------------------

data VectorPlot v n = VectorPlot
   { _vectorV        :: v n
   , _vectorPoint   :: (n,n)
   , _vectorArrows  :: ArrowOpts n
   } deriving Typeable

makeLenses ''VectorPlot

type instance V (VectorPlot v n) = v
type instance N (VectorPlot v n) = n

instance (Metric v, OrderedField n) => Enveloped (VectorPlot v n) where
  getEnvelope = const mempty

instance (TypeableFloat n, Renderable (Path V2 n) b)
    => Plotable (VectorPlot V2 n) b where
  renderPlotable s _sty v = arrowAt' opts pt1 (V2 q r)
                          # transform (s^.specTrans)
                          # translate (r2 (x, y))
                          where
                             pt1      = p2 (x, y)
                             (x, y)   = v ^. vectorPoint
                             (V2 q r) = v ^. vectorV
                             opts     = v ^. vectorArrows

  defLegendPic sty VectorPlot {..}
      = (p2 (-10,0) ~~ p2 (10,0))
          # applyLineStyle sty

-- | Plot a given vector at (0,0).
mkVectorPlot :: TypeableFloat n => v n -> VectorPlot v n
mkVectorPlot f
  = VectorPlot
      { _vectorV       = f
      , _vectorPoint   = (0,0)
      , _vectorArrows  = def
      }

-- | Plot a given vector at a given point.
mkVectorPointPlot :: TypeableFloat n => v n -> (n, n) ->VectorPlot v n
mkVectorPointPlot f d
  = VectorPlot
      { _vectorV       = f
      , _vectorPoint   = d
      , _vectorArrows  = def
      }

------------------------------------------------------------------------
-- Vector plot lenses
------------------------------------------------------------------------
class HasVector a v n | a -> v n where
  vector :: Lens' a (VectorPlot v n)

  setArrowOpts :: Lens' a (ArrowOpts n)
  setArrowOpts = vector . lens _vectorArrows (\s b -> (s {_vectorArrows = b}))

instance HasVector (VectorPlot v n) v n where
  vector = id

instance HasVector (Plot (VectorPlot v n) b) v n where
  vector = rawPlot

------------------------------------------------------------------------
-- Parametric Plot
------------------------------------------------------------------------

parametricPlot
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (ParametricPlot v n) b,
      TypeableFloat n)
  => (n -> p) -> State (Plot (ParametricPlot v n) b) () -> m ()
parametricPlot f = addPlotable (mkParametricPlot f)

parametricPlot'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (ParametricPlot v n) b,
      TypeableFloat n)
  => (n -> p) -> m ()
parametricPlot' f = addPlotable' (mkParametricPlot f)

-- parametricPlotL
--   :: (v ~ BaseSpace c,
--       PointLike v n p,
--       MonadState (Axis b c n) m,
--       Plotable (ParametricPlot v n) b,
--       Additive v, TypeableFloat n)
--   => String -> (n -> p) -> m ()
-- parametricPlotL l f = addPlotableL l (mkParametricPlot f)

-- range variant

parametricRangePlot
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (ParametricPlot v n) b)
  => (n -> p) -> (n ,n) -> State (Plot (ParametricPlot v n) b) () -> m ()
parametricRangePlot f d = addPlotable (mkParametricRangePlot f d)

parametricRangePlot'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (ParametricPlot v n) b)
  => (n -> p) -> (n ,n) -> m ()
parametricRangePlot' f d = addPlotable' (mkParametricRangePlot f d)

-- parametricRangePlotL
--   :: (v ~ BaseSpace c,
--       PointLike v n p,
--       MonadState (Axis b c n) m,
--       Plotable (ParametricPlot v n) b,
--       Additive v, TypeableFloat n)
--   => String -> (n -> p) -> (n ,n) -> m ()
-- parametricRangePlotL l f d = addPlotableL l (mkParametricRangePlot f d)

------------------------------------------------------------------------
-- Vector Plot
------------------------------------------------------------------------

vectorPlot
  :: (v ~ BaseSpace c,
      MonadState (Axis b c n) m,
      Plotable (VectorPlot v n) b,
      TypeableFloat n)
  =>  v n -> State (Plot (VectorPlot v n) b) () -> m ()
vectorPlot f = addPlotable (mkVectorPlot f)

vectorPointPlot
  :: (v ~ BaseSpace c,
      MonadState (Axis b c n) m,
      Plotable (VectorPlot v n) b,
      TypeableFloat n)
  =>  v n -> (n, n) -> State (Plot (VectorPlot v n) b) () -> m ()
vectorPointPlot f d = addPlotable (mkVectorPointPlot f d)

vectorPointPlot'
  :: (v ~ BaseSpace c,
      MonadState (Axis b c n) m,
      Plotable (VectorPlot v n) b,
      TypeableFloat n)
  =>  v n -> (n, n) -> m ()
vectorPointPlot' f d = addPlotable' (mkVectorPointPlot f d)

vectorPointPlot''
  :: (v ~ BaseSpace c,
      MonadState (Axis b c n) m,
      Plotable (VectorPlot v n) b,
      TypeableFloat n)
  =>  v n -> (n, n) -> ArrowOpts n -> m ()
vectorPointPlot'' f d opts = addPlotable (mkVectorPointPlot f d) $ do
                              setArrowOpts .= opts

-- vectorPointPlotL
--   :: (v ~ BaseSpace c,
--       MonadState (Axis b c n) m,
--       Plotable (VectorPlot v n) b,
--       Additive v, TypeableFloat n)
--   =>  String -> v n -> (n, n) -> m ()
-- vectorPointPlotL l f d = addPlotableL l (mkVectorPointPlot f d)

vectorFieldPlot
  :: (v ~ BaseSpace c,
      MonadState (Axis b c n) m,
      Plotable (VectorPlot v n) b,
      TypeableFloat n)
  =>  [v n] -> [(n, n)] -> ArrowOpts n -> m ()
vectorFieldPlot vs ps opts =
  F.forM_ (zip vs ps) $ \x ->
    vectorPointPlot'' (fst x) (snd x) opts

-------------------------------------------------------------------------------
-- Line
-------------------------------------------------------------------------------

abLinePlot
  :: (v ~ BaseSpace c,
      PointLike v n (P2 n),
      MonadState (Axis b c n) m,
      Plotable (ParametricPlot v n) b,
      Additive v, TypeableFloat n)
     => n -> n -> (n ,n) -> State (Plot (ParametricPlot v n) b) () -> m ()
abLinePlot slope intercept d = addPlotable (mkParametricRangePlot (createABLine slope intercept) d)

hLinePlot
  :: (v ~ BaseSpace c,
      PointLike v n (P2 n),
      MonadState (Axis b c n) m,
      Plotable (ParametricPlot v n) b,
      Additive v, TypeableFloat n)
     => n -> (n ,n) -> State (Plot (ParametricPlot v n) b) () -> m ()
hLinePlot intercept d = addPlotable (mkParametricRangePlot (createHLine intercept) d)

vLinePlot
  :: (v ~ BaseSpace c,
      PointLike v n (P2 n),
      MonadState (Axis b c n) m,
      Plotable (ParametricPlot v n) b,
      Additive v, TypeableFloat n)
     => n -> (n ,n) -> State (Plot (ParametricPlot v n) b) () -> m ()
vLinePlot intercept d = addPlotable (mkParametricRangePlot (createVLine intercept) d)

------------------------------------------------------------------------

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

------------------------------------------------------------------------
-- Mesh plot
------------------------------------------------------------------------

-- data MeshPlot n = MeshPlot
--   { _meshFunction    :: n -> n -> n -- ^ $\x y -> z$
--   , _meshDomain      :: V2 (n, n)
--   -- , _meshPlotOptions :: FunctionPlotOptions b R3
--   } deriving Typeable

-- type instance V (MeshPlot n) = V3
-- type instance N (MeshPlot n) = n

-- makeLenses ''MeshPlot


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
