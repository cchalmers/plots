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

module Plots.Types.Density
  (

-- * GDensitylot plot
     GDensityPlot
  , _DensityPlot

  , DensityPlot
  , mkDensityPlotOf
  , mkDensityPlot

  , densityY1
  , fillArea

  ) where

import           Control.Lens                    hiding (lmap, none, transform,
                                                  ( # ))
import qualified Data.Foldable                   as F
import           Data.Typeable
import           Data.List
import           Data.Function

import           Diagrams.Prelude
import           Diagrams.Trail

import           Diagrams.Coordinates.Isomorphic

import           Plots.Themes
import           Plots.Types

data GDensityPlot v n a = forall s. GDensityPlot
  { dData  :: s
  , dFold  :: Fold s a
  , dPos   :: a -> Point v n
  , dFunc  :: [P2 n] -> Located (Trail' Line V2 n)
  , dFill  :: Bool
  } deriving Typeable

type instance V (GDensityPlot v n a) = v
type instance N (GDensityPlot v n a) = n

instance (Metric v, OrderedField n) => Enveloped (GDensityPlot v n a) where
  getEnvelope GDensityPlot {..} = foldMapOf (dFold . to dPos) getEnvelope dData

instance (Typeable a, Typeable b, TypeableFloat n, Renderable (Path V2 n) b, Enum n)
    => Plotable (GDensityPlot V2 n a) b where
  renderPlotable s GDensityPlot {..} pp = 
               dd # transform t
                  # stroke
--                  check smooth for more details
--                  # applyLineStyle pp  
            <> if dFill 
                then (fillDensity dd) # stroke
                                      # lw none
                                      # applyBarStyle pp
                                      # transform t
                else mempty
          where
            ps             = toListOf (dFold . to dPos . to (logPoint ls)) dData
            dd             = dFunc ps
            t              = s ^. specTrans
            ls             = s ^. specScale

  defLegendPic GDensityPlot {..} pp
      = (p2 (-10,0) ~~ p2 (10,0))
          # applyLineStyle pp

------------------------------------------------------------------------
-- Simple Density Plot
------------------------------------------------------------------------

type DensityPlot v n = GDensityPlot v n (Point v n)

mkDensityPlot :: (PointLike v n p, F.Foldable f, Ord n, Floating n, Enum n, Num n)
              => f p -> DensityPlot v n
mkDensityPlot = mkDensityPlotOf folded


mkDensityPlotOf :: (PointLike v n p, Ord n, Floating n, Enum n, Num n)
                => Fold s p -> s -> DensityPlot v n
mkDensityPlotOf f a = GDensityPlot
  { dData = a
  , dFold = f . unpointLike
  , dPos  = id
  , dFunc = densityY1
  , dFill = False 
  }

_DensityPlot :: (Plotable (DensityPlot v n) b, Typeable b)
                   => Prism' (Plot b v n) (DensityPlot v n)
_DensityPlot = _Plot

---------- add more of this function - one for mean other for sum --

densityY1 :: (Ord n, Floating n, Enum n) => [P2 n] -> Located (Trail' Line V2 n)
densityY1 xs = fromVertices (map p2 [(1.0,0.5), (2.0,1.0), (3.0,2.1), (4.0, 5.0), (4.5,3.2), (5.1,1.4)])

fillDensity :: (Ord n, Fractional n, Enum n) => Located (Trail' Line V2 n) -> Located (Trail' Loop V2 n)
fillDensity dd = dd # mapLoc closeLine

----------------------------------------------------------------------------
-- Density Lenses
----------------------------------------------------------------------------

class HasDensity a v n d | a -> v n, a -> d where
  density :: Lens' a (GDensityPlot v n d)

  fillArea :: Lens' a Bool
  fillArea = density . lens dFill (\df fill -> df {dFill = fill})

instance HasDensity (GDensityPlot v n d) v n d where
  density = id

instance HasDensity (PropertiedPlot (GDensityPlot v n d) b) v n d where
  density = _pp

