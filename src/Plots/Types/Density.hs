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
{-# LANGUAGE AllowAmbiguousTypes       #-}

{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

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

    -- * Density plot
  , densityPlot
  , densityPlot'
  , densityPlotL

     -- * Fold variant density plot
   , densityPlotOf
   , densityPlotOf'
   , densityPlotOfL
  ) where

import               Control.Lens                    hiding (lmap, none, transform,
                                                      ( # ))
import           Control.Monad.State.Lazy
import qualified     Data.Foldable                   as F
import               Data.Typeable
import               Data.List
import               Data.Function

import               Diagrams.Coordinates.Isomorphic
import               Diagrams.Prelude

import               Plots.Style
import               Plots.Types
import               Plots.Axis
import               Plots.API

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
                                      # applyAreaStyle pp
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

------------------------------------------------------------------------
-- Density Plot
------------------------------------------------------------------------

-- $ density plot
-- Density plots display data as average x density of given points,
-- Box plots have the following lenses:
--
-- @
-- * 'fillArea' :: 'Lens'' ('DensityPlot' v n) 'Bool' - False
-- @

-- | Add a 'DenistyPlot' to the 'AxisState' from a data set.
--
-- @
--   myaxis = r2Axis ~&
--     densityPlot data1
-- @
--
-- === __Example__
--
-- <<plots/density.png#diagram=density&width=300>>
--
-- @
-- myaxis :: Axis B V2 Double
-- myaxis = r2Axis &~ do
--
--     densityPlotL mydata1
--     densityPlotL mydata2
--     densityPlotL mydata3
-- @

densityPlot
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (DensityPlot v n) b,
      F.Foldable f ,
      Enum n, TypeableFloat n)
  => f p -> m ()
densityPlot d = addPlotable (mkDensityPlot d)

-- | Make a 'DensityPlot' and take a 'State' on the plot to alter it's
--   options
--
-- @
--   myaxis = r2Axis &~ do
--     densityPlot' pointData1 $ do
--       fillArea .= True
--       addLegendEntry "data 1"
-- @

densityPlot'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (DensityPlot v n) b,
      F.Foldable f ,
      Enum n, TypeableFloat n)
  => f p -> PlotState (DensityPlot v n) b -> m ()
densityPlot' d = addPlotable' (mkDensityPlot d)

-- | Add a 'DensityPlot' with the given name for the legend entry.
--
-- @
--   myaxis = r2Axis &~ do
--     densityPlotL "blue team" pointData1
--     densityPlotL "red team" pointData2
-- @

densityPlotL
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (DensityPlot v n) b,
      F.Foldable f ,
      Enum n, TypeableFloat n)
  => String -> f p  -> m ()
densityPlotL l d = addPlotableL l (mkDensityPlot d)

-- fold variant

densityPlotOf
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (DensityPlot v n) b,
      Enum n, TypeableFloat n)
  => Fold s p -> s -> m ()
densityPlotOf f s = addPlotable (mkDensityPlotOf f s)

densityPlotOf'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (DensityPlot v n) b,
      Enum n, TypeableFloat n)
  => Fold s p -> s -> PlotState (DensityPlot v n) b -> m ()
densityPlotOf' f s = addPlotable' (mkDensityPlotOf f s)

densityPlotOfL
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (DensityPlot v n) b,
      Enum n, TypeableFloat n)
  => String -> Fold s p -> s -> m ()
densityPlotOfL l f s = addPlotableL l (mkDensityPlotOf f s)


