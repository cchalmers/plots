{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TypeFamilies              #-}

module Plots.Types.Ribbon
  (

-- * GRibbonPlot plot
     GRibbonPlot
  , _RibbonPlot

  , RibbonPlot
  , mkRibbonPlotOf
  , mkRibbonPlot

  , strokeEdge

  ) where

import           Control.Lens                    hiding (lmap, none, transform,
                                                  ( # ))
import qualified Data.Foldable                   as F
import           Data.Typeable
import           Diagrams.Prelude

import           Diagrams.Coordinates.Isomorphic

import           Plots.Themes
import           Plots.Types

data GRibbonPlot v n a = forall s. GRibbonPlot
  { sData :: s
  , sFold :: Fold s a
  , sPos  :: a -> Point v n
  , sLine :: Bool
  } deriving Typeable

type instance V (GRibbonPlot v n a) = v
type instance N (GRibbonPlot v n a) = n

instance (Metric v, OrderedField n) => Enveloped (GRibbonPlot v n a) where
  getEnvelope GRibbonPlot {..} = foldMapOf (sFold . to sPos) getEnvelope sData

instance (Typeable a, Typeable b, TypeableFloat n, Renderable (Path V2 n) b)
    => Plotable (GRibbonPlot V2 n a) b where
  renderPlotable s GRibbonPlot {..} pp =
      fromVertices ps
        # mapLoc closeLine
        # stroke
        # lw none
        # applyBarStyle pp
        # transform t

   <> if sLine
        then fromVertices ps
               # mapLoc closeLine
               # stroke
               # transform t
               # applyLineStyle pp
        else mempty
    where
      ps = toListOf (sFold . to sPos . to (logPoint ls)) sData
      t  = s ^. specTrans
      ls = s ^. specScale

  defLegendPic GRibbonPlot {..} pp
      = square 5 # applyBarStyle pp

_RibbonPlot :: (Plotable (RibbonPlot v n) b, Typeable b)
             => Prism' (Plot b v n) (RibbonPlot v n)
_RibbonPlot = _Plot

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
  , sLine = True
  }

------------------------------------------------------------------------
-- Ribbon lenses
------------------------------------------------------------------------

class HasRibbon a v n d | a -> v n, a -> d where
  ribbon :: Lens' a (GRibbonPlot v n d)

  strokeEdge :: Lens' a Bool
  strokeEdge = ribbon . lens sLine (\s b -> (s {sLine = b}))

instance HasRibbon (GRibbonPlot v n d) v n d where
  ribbon = id

instance HasRibbon (PropertiedPlot (GRibbonPlot v n d) b) v n d where
  ribbon = _pp

