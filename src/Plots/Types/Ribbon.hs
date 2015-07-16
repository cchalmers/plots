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

module Plots.Types.Ribbon
  (

-- * GRibbonPlot plot
     GRibbonPlot
  , _RibbonPlot

  , RibbonPlot
  , mkRibbonPlotOf
  , mkRibbonPlot
  
  , strokeEdge
  , fillOpacity
  
--  , StepPlot
--  , mkStepPlotOf
--  , mkStepPlot

--  , mkGLinePlotOf
-- , mkGLinePlot

--  , dotsonPoint
--  , fillStyle
--  , strokePath
  ) where

import           Control.Lens     hiding (transform, ( # ), lmap, none)
import qualified Data.Foldable    as F
import           Data.Typeable
import           Diagrams.Coordinates.Isomorphic
-- import Data.Typeable
import           Diagrams.Prelude  hiding (view)
-- import Diagrams.LinearMap
-- import Diagrams.ThreeD.Types

import          Diagrams.Coordinates.Isomorphic

import          Plots.Themes
import          Plots.Types

data GRibbonPlot v n a = forall s. GRibbonPlot
  { sData :: s
  , sFold :: Fold s a
  , sPos  :: a -> Point v n
  , sOpa  :: Double
  , sLine :: Bool
  } deriving Typeable

type instance V (GRibbonPlot v n a) = v
type instance N (GRibbonPlot v n a) = n

instance (Metric v, OrderedField n) => Enveloped (GRibbonPlot v n a) where
  getEnvelope GRibbonPlot {..} = foldMapOf (sFold . to sPos) getEnvelope sData

instance (Typeable a, Typeable b, TypeableFloat n, Renderable (Path V2 n) b)
    => Plotable (GRibbonPlot V2 n a) b where
  renderPlotable s GRibbonPlot {..} pp =
      fromVertices (toListOf (sFold . to sPos . to (logPoint ls)) sData)
        # closeLine 
        # strokeLoop
        # lw none
        # applyBarStyle pp
        # translate  (r2 (unp2 (firstinlist (toListOf (sFold . to sPos . to (logPoint ls)) sData))))
        # transform t
        # opacity sOpa
        
   <> if sLine
        then fromVertices (toListOf (sFold . to sPos . to (logPoint ls)) sData)
               # transform t
               # applyLineStyle pp
        else mempty
    where
      t = s ^. specTrans
      ls = s ^. specScale
      marker = pp ^. plotMarker

  defLegendPic GRibbonPlot {..} pp 
      = fromVertices [p2 (-2.5,2.5) , p2 (-2.5,-2.5), p2 (2.5,-2.5), p2 (2.5,2.5)]
          # closeLine 
          # strokeLoop 
          # lw none
          # applyBarStyle pp

_RibbonPlot :: (Plotable (RibbonPlot v n) b, Typeable b)
             => Prism' (Plot b v n) (RibbonPlot v n)
_RibbonPlot = _Plot

firstinlist :: [a] -> a
firstinlist (x:xs) = x
firstinlist [] = error "something"
------------------------------------------------------------------------
-- Ribbon Plot
------------------------------------------------------------------------

type RibbonPlot v n = GRibbonPlot v n (Point v n)

-- | Make a line plot.
mkRibbonPlot :: (PointLike v n p, F.Foldable f, Num n)
              => f p -> RibbonPlot v n
mkRibbonPlot = mkRibbonPlotOf folded

-- | Make a line plot using the given fold.
mkRibbonPlotOf :: (PointLike v n p, Num n)
                => Fold s p -> s -> RibbonPlot v n
mkRibbonPlotOf f a = GRibbonPlot
  { sData = a
  , sFold = f . unpointLike
  , sPos  = id
  , sOpa  = 1
  , sLine = True
  }

------------------------------------------------------------------------
-- Ribbon lenses
------------------------------------------------------------------------

class HasRibbon a v n d | a -> v n, a -> d where
  ribbon :: Lens' a (GRibbonPlot v n d)

  fillOpacity  :: Lens' a Double
  fillOpacity  =  ribbon . lens sOpa (\sp opa -> sp {sOpa = opa})

  strokeEdge :: Lens' a Bool
  strokeEdge = ribbon . lens sLine (\s b -> (s {sLine = b}))

instance HasRibbon (GRibbonPlot v n d) v n d where
  ribbon = id

instance HasRibbon (PropertiedPlot (GRibbonPlot v n d) b) v n d where
  ribbon = _pp

