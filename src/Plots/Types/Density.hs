{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE FunctionalDependencies    #-}

{-# LANGUAGE StandaloneDeriving        #-}

module Plots.Types.Density
  ( -- * GDensitylot plot
    GDensityPlot
  , _DensityPlot

    -- * Density plot
  , DensityPlot
  , mkDensityPlotOf
  , mkDensityPlot
    
    -- * Helper funtions
  , densityY

    -- * Lenses
  , fillArea
  ) where

import               Control.Lens                    hiding (lmap, none, transform,
                                                      ( # ))
import qualified     Data.Foldable                   as F
import               Data.Typeable
import               Data.List
import               Data.Function

import               Diagrams.Coordinates.Isomorphic
import               Diagrams.Prelude

import               Plots.Themes
import               Plots.Types

------------------------------------------------------------------------
-- General density plot
------------------------------------------------------------------------

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

-- having problems using applyLineStyle to dd.
-- dd :: Located (Trail' Line V2 n)
-- # applyLineStyle pp

  defLegendPic GDensityPlot {..} pp
      = (p2 (-10,0) ~~ p2 (10,0))
          # applyLineStyle pp

_DensityPlot :: (Plotable (DensityPlot v n) b, Typeable b)
                   => Prism' (Plot b v n) (DensityPlot v n)
_DensityPlot = _Plot

------------------------------------------------------------------------
-- Simple density plot
------------------------------------------------------------------------

type DensityPlot v n = GDensityPlot v n (Point v n)

-- | Make a density plot.
mkDensityPlot :: (PointLike v n p, F.Foldable f, Ord n, Floating n, Enum n, Num n)
              => f p -> DensityPlot v n
mkDensityPlot = mkDensityPlotOf folded

-- | Make a density plot using a given fold.
mkDensityPlotOf :: (PointLike v n p, Ord n, Floating n, Enum n, Num n)
                => Fold s p -> s -> DensityPlot v n
mkDensityPlotOf f a = GDensityPlot
  { dData = a
  , dFold = f . unpointLike
  , dPos  = id
  , dFunc = densityY
  , dFill = False
  }

----------------------------------------------------------------------------
-- Helper functions
----------------------------------------------------------------------------

-- | Function used to create the density, takes the average of the xdata, bin y = 10.
densityY :: (Ord n, Floating n, Enum n) => [P2 n] -> Located (Trail' Line V2 n)
densityY xs = cubicSpline False (map p2 (zip xpts ypts))
  where
    xmin = fst (maximumBy (compare `on` fst) (map unp2 xs))
    xmax = fst (minimumBy (compare `on` fst) (map unp2 xs))
    xpts = [xmin, (xmin + w) .. xmax]
    ypts = [bin1D xs (xpt, (xpt + w)) | xpt <- xpts]
    w    = (xmax - xmin)/ 10.0

    bin1D as (a,b) = mean [y | (x,y) <- (map unp2 as), x > b, x < a]

-- need to add more density functions

mean :: (Num a, Fractional a) => [a] -> a
mean [] = 0.0
mean xs = (sum xs)/ fromIntegral (length xs)

fillDensity :: (Ord n, Fractional n, Enum n) => Located (Trail' Line V2 n) -> Located (Trail' Loop V2 n)
fillDensity dd = dd # mapLoc closeLine

-- for better density fill, extend dd till xmin or zero
-- dd :: Located (Trail' Line V2 n)

----------------------------------------------------------------------------
-- Density plot lenses
----------------------------------------------------------------------------

class HasDensity a v n d | a -> v n, a -> d where
  density :: Lens' a (GDensityPlot v n d)

  fillArea :: Lens' a Bool
  fillArea = density . lens dFill (\df fill -> df {dFill = fill})

instance HasDensity (GDensityPlot v n d) v n d where
  density = id

instance HasDensity (PropertiedPlot (GDensityPlot v n d) b) v n d where
  density = _pp

