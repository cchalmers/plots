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

module Plots.Types.Boxplot
  (

-- * GDensitylot plot
     GBoxPlot
  , _BoxPlot

  , BoxPlot
  , mkBoxPlotOf
  , mkBoxPlot

  , fillBox

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

data BP = BP
   { bppoint :: (Double, Double)
   , bpw     :: Double
   , bph1    :: Double
   , bph2    :: Double
   }

-- need to change this part so that it have colour variable width at each point
-- also can be uneven at different part so we can make errorbar, crossbar, 2-boxplot etc..

data GBoxPlot v n a = forall s. GBoxPlot
  { bData  :: s
  , bFold  :: Fold s a
  , bPos   :: a -> Point v n
  , bBox  :: [P2 Double] -> BP
  , bFill  :: Bool
  } deriving Typeable

type instance V (GBoxPlot v n a) = v
type instance N (GBoxPlot v n a) = n

instance (Metric v, OrderedField n) => Enveloped (GBoxPlot v n a) where
  getEnvelope GBoxPlot {..} = foldMapOf (bFold . to bPos) getEnvelope bData

instance (Typeable a, Typeable b, TypeableFloat n, Renderable (Path V2 n) b, Enum n, n ~ Double)
    => Plotable (GBoxPlot V2 n a) b where
  renderPlotable s GBoxPlot {..} pp = 
               if bFill 
                then mconcat ([ draw' d | d <-(drawBoxPlot dd)] ++ [foo])
               else mconcat [ draw' d | d <-(drawBoxPlot dd)]
          where
            ps             = toListOf (bFold . to bPos . to (logPoint ls)) bData
            dd             = bBox ps
            foo            = (makeRect dd) # mapLoc closeLine
                                           # stroke
                                           # lw none
                                           # applyBarStyle pp
                                           # transform t
            t              = s ^. specTrans
            ls             = s ^. specScale
            draw' d        = d # transform t
                               # stroke 
                               

  defLegendPic GBoxPlot {..} pp
      = square 5 # applyBarStyle pp

------------------------------------------------------------------------
-- Simple Density Plot
------------------------------------------------------------------------

type BoxPlot v n = GBoxPlot v n (Point v n)

mkBoxPlot :: (PointLike v n p, F.Foldable f, Ord n, Floating n, Enum n, Num n)
              => f p -> BoxPlot v n
mkBoxPlot = mkBoxPlotOf folded

mkBoxPlotOf :: (PointLike v n p, Ord n, Floating n, Enum n, Num n)
                => Fold s p -> s -> BoxPlot v n
mkBoxPlotOf f a = GBoxPlot
  { bData = a
  , bFold = f . unpointLike
  , bPos  = id
  , bBox  = boxplotstat
  , bFill = True 
  }

_BoxPlot :: (Plotable (BoxPlot v n) b, Typeable b)
                   => Prism' (Plot b v n) (BoxPlot v n)
_BoxPlot = _Plot

---------- add more of this function - one for mean other for sum --

boxplotstat :: (Ord n, Floating n, Enum n, n ~ Double) => [P2 n] -> BP
boxplotstat ps = BP
   { bppoint = meanXY
   , bpw  = maxX * 0.3
   , bph1 = maxY * 0.5
   , bph2 = maxY * 0.8
   }
   where 
     xs     = [fst (unp2 p) | p <- ps]
     ys     = [snd (unp2 p) | p <- ps]
     meanXY = ((mean xs), (mean ys))
     maxX   = maximum xs - (mean xs)
     maxY   = maximum ys - (mean ys)

mean :: (Real a, Fractional b) => [a] -> b
mean xs = realToFrac (sum xs)/ genericLength xs

drawBoxPlot :: BP -> [Located (Trail' Line V2 Double)]
drawBoxPlot (BP (x,y) w h1 h2) = [a, b ,c ,d ,e]
                                 where
                                   xmin  = x - w/2
                                   xmax  = x + w/2
                                   y1min = y - h1
                                   y2min = y - h2
                                   y1max = y + h1
                                   y2max = y + h2 
                                   a     = fromVertices (map p2 [(xmin,y1max),(xmax,y1max),(xmax,y1min),(xmin,y1min)])
                                   b     = fromVertices (map p2 [(xmin,y1max),(xmin,y1min)])
                                   c     = fromVertices (map p2 [(xmin,y),(xmax,y)])
                                   d     = fromVertices (map p2 [(x,y1min),(x,y2min)])
                                   e     = fromVertices (map p2 [(x,y1max),(x,y2max)])

makeRect :: BP -> Located (Trail' Line V2 Double)
makeRect  (BP (x,y) w h1 h2) = fromVertices (map p2 [(xmin,y1max),(xmax,y1max),(xmax,y1min),(xmin,y1min)])
                                 where
                                   xmin  = x - w/2
                                   xmax  = x + w/2
                                   y1min = y - h1
                                   y2min = y - h2
                                   y1max = y + h1
                                   y2max = y + h2
----------------------------------------------------------------------------
-- Density Lenses
----------------------------------------------------------------------------

class HasBox a v n d | a -> v n, a -> d where
  box :: Lens' a (GBoxPlot v n d)

  fillBox :: Lens' a Bool
  fillBox = box . lens bFill (\df fill -> df {bFill = fill})

instance HasBox (GBoxPlot v n d) v n d where
  box = id

instance HasBox (PropertiedPlot (GBoxPlot v n d) b) v n d where
  box = _pp

