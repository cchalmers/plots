{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ConstraintKinds       #-}

{-# OPTIONS_GHC -fno-warn-orphans       #-}

{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE RecordWildCards           #-}

{-# LANGUAGE StandaloneDeriving        #-}

-- Orphans: Plotable (Path V2 n)

module Plots.Types.Line
  ( -- * Trail plot
    mkTrail
  , mkTrailOf

  , mkPath
  , mkPathOf

    -- * GLinePlot plot
  , GLinePlot
  , _LinePlot

  , LinePlot
  , mkLinePlotOf
  , mkLinePlot
  
  , StepPlot
  , mkStepPlotOf
  , mkStepPlot

  , mkGLinePlotOf
  , mkGLinePlot

  , dotsonPoint
  , pathStyle
  ) where

import           Control.Lens     hiding (transform, ( # ), lmap)
import qualified Data.Foldable    as F
import           Data.Typeable
import           Diagrams.Coordinates.Isomorphic
-- import Data.Typeable
import           Diagrams.Prelude  hiding (view)
-- import Diagrams.LinearMap
-- import Diagrams.ThreeD.Types

import Diagrams.Coordinates.Isomorphic

import Plots.Themes
import Plots.Types

------------------------------------------------------------------------
-- mkTrail and mkPath
------------------------------------------------------------------------

mkTrail :: (PointLike v n p, OrderedField n, F.Foldable f) => f p -> Located (Trail v n)
mkTrail = mkTrailOf folded

mkTrailOf :: (PointLike v n p, OrderedField n) => Fold s p -> s -> Located (Trail v n)
mkTrailOf f ps = fromVertices $ toListOf (f . unpointLike) ps

mkPathOf :: (PointLike v n p, OrderedField n) => Fold s t -> Fold t p -> s -> Path v n
mkPathOf f1 f2 as = Path $ map (mkTrailOf f2) (toListOf f1 as)

mkPath :: (PointLike v n p, OrderedField n, F.Foldable f, F.Foldable g) => g (f p) -> Path v n
mkPath = mkPathOf folded folded

instance (TypeableFloat n, Renderable (Path V2 n) b) => Plotable (Path V2 n) b where
  renderPlotable s path pp
    = stroke path
        # transform (s^.specTrans)
        # applyLineStyle pp

  defLegendPic _ pp
    = (p2 (-10,0) ~~ p2 (10,0))
        # applyLineStyle pp

------------------------------------------------------------------------
-- Sample
------------------------------------------------------------------------

-- kernalDensity :: Int -> Fold s n -> s -> LinePlot V2 n
-- kernalDensity n f as =
-----------------------------------------------------------------------

data GLinePlot v n a = forall s. GLinePlot
  { sData :: s
  , sFold :: Fold s a
  , sPos  :: a -> Point v n
  , sSty  :: Maybe (a -> Style V2 n)
  , cPnt  :: Bool
  } deriving Typeable

type instance V (GLinePlot v n a) = v
type instance N (GLinePlot v n a) = n

instance (Metric v, OrderedField n) => Enveloped (GLinePlot v n a) where
  getEnvelope GLinePlot {..} = foldMapOf (sFold . to sPos) getEnvelope sData

instance (Typeable a, Typeable b, TypeableFloat n, Renderable (Path V2 n) b)
    => Plotable (GLinePlot V2 n a) b where
  renderPlotable s GLinePlot {..} pp =
      fromVertices (toListOf (sFold . to sPos . to (logPoint ls)) sData)
        # transform t
        # applyLineStyle pp
   <> if cPnt
        then foldMapOf sFold mk sData # applyMarkerStyle pp
        else mempty
    where
      t = s ^. specTrans
      ls = s ^. specScale
      mk a = marker # maybe id (applyStyle . ($ a)) sSty
                    # moveTo (specPoint s $ sPos a)
      marker = pp ^. plotMarker

  defLegendPic GLinePlot {..} pp 
      = (p2 (-10,0) ~~ p2 (10,0))
          # applyLineStyle pp

_LinePlot :: (Plotable (LinePlot v n) b, Typeable b)
             => Prism' (Plot b v n) (LinePlot v n)
_LinePlot = _Plot

------------------------------------------------------------------------
-- Line Plot
------------------------------------------------------------------------

type LinePlot v n = GLinePlot v n (Point v n)

-- | Make a line plot.
mkLinePlot :: (PointLike v n p, F.Foldable f, Num n)
              => f p -> LinePlot v n
mkLinePlot = mkLinePlotOf folded

-- | Make a line plot using the given fold.
mkLinePlotOf :: (PointLike v n p, Num n)
                => Fold s p -> s -> LinePlot v n
mkLinePlotOf f a = GLinePlot
  { sData = a
  , sFold = f . unpointLike
  , sPos  = id
  , sSty  = Nothing
  , cPnt  = False
  }

------------------------------------------------------------------------
-- Step plot -- use fold here rather than api
------------------------------------------------------------------------

type StepPlot v n = GLinePlot v n (Point v n)

mkStepPlotOf :: (PointLike v n p, Fractional n)
               => Fold s p -> s -> StepPlot v n
mkStepPlotOf f a = GLinePlot
  { sData = a
  , sFold = f . unpointLike
  , sPos  = id
  , sSty  = Nothing
  , cPnt  = False
  }

mkStepPlot :: (PointLike v n p, F.Foldable f, Fractional n)
             => f p -> StepPlot v n
mkStepPlot = mkStepPlotOf folded

------------------------------------------------------------------------
-- General Line Plot
------------------------------------------------------------------------

mkGLinePlotOf :: (PointLike v n p, Fractional n)
                 => Fold s a -> s -> (a -> p) -> GLinePlot v n a
mkGLinePlotOf f a pf = GLinePlot
  { sData = a
  , sFold = f
  , sPos  = view unpointLike . pf
  , sSty  = Nothing
  , cPnt = False
  }

mkGLinePlot :: (PointLike v n p, F.Foldable f, Fractional n)
               => f a -> (a -> p) -> GLinePlot v n a
mkGLinePlot = mkGLinePlotOf folded

------------------------------------------------------------------------
-- Line plot lenses
------------------------------------------------------------------------

class HasPath a v n d | a -> v n, a -> d where
  line :: Lens' a (GLinePlot v n d)

  pathStyle :: Lens' a (Maybe (d -> Style V2 n))
  pathStyle  = line . lens sSty (\sp sty -> sp {sSty = sty})


  dotsonPoint :: Lens' a Bool
  dotsonPoint = line . lens cPnt (\s b -> (s {cPnt = b}))

instance HasPath (GLinePlot v n d) v n d where
  line = id

instance HasPath (PropertiedPlot (GLinePlot v n d) b) v n d where
  line = _pp

