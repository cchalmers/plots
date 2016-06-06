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
  ( -- * Adding box plots
    boxPlot
  , boxPlot'
  -- , boxPlotL

    -- * Fold variant boxplot
  , boxPlotOf
  , boxPlotOf'
  -- , boxPlotOfL

     -- * Boxplot type
  , GBoxPlot
  -- , _BoxPlot

    -- * Box plot
  , BoxPlot
  , mkBoxPlotOf
  , mkBoxPlot

    -- * Lenses
  , fillBox

  ) where

import           Control.Lens                    hiding (lmap, none, transform,
                                                  ( # ))
import           Control.Monad.State.Lazy
import qualified Data.Foldable                   as F
import           Data.Typeable
import           Data.List

import           Diagrams.Prelude
import           Diagrams.Coordinates.Isomorphic

import           Plots.Style
import           Plots.Types
import           Plots.Axis
import           Plots.Axis.Scale

------------------------------------------------------------------------
-- Boxplot data
------------------------------------------------------------------------

data BP = BP
   { bppoint :: (Double, Double)
   , bpw     :: Double
   , bph1    :: Double
   , bph2    :: Double
   }

-- need to change this part so that it have variable colour,
-- width at each point, if done properly this can be a base for
-- errorbar, crossbar, 2-boxplot and so on.

------------------------------------------------------------------------
-- General boxplot
------------------------------------------------------------------------

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

instance (Typeable a, TypeableFloat n, Renderable (Path V2 n) b, n ~ Double)
    => Plotable (GBoxPlot V2 n a) b where
  renderPlotable s sty GBoxPlot {..} =
    if bFill
      then mconcat ([ draw' d | d <-(drawBoxPlot dd)] ++ [foo])
      else mconcat [ draw' d | d <-(drawBoxPlot dd)]
    where
      ps      = toListOf (bFold . to bPos . to (logPoint ls)) bData
      dd      = bBox ps
      foo     = makeRect dd
                  # mapLoc closeLine
                  # stroke
                  # lw none
                  # applyAreaStyle sty
                  # transform t
      t       = s ^. specTrans
      ls      = s ^. specScale
      draw' d = d # transform t
                  # stroke

  defLegendPic sty GBoxPlot {..}
      = square 5 # applyAreaStyle sty

-- _BoxPlot :: (Plotable (BoxPlot v n) b, Typeable b)
--                    => Prism' (Plot b v n) (BoxPlot v n)
-- _BoxPlot = _Plot

------------------------------------------------------------------------
-- Boxplot
------------------------------------------------------------------------

type BoxPlot v n = GBoxPlot v n (Point v n)

-- | Draw a boxplot with the given data.
mkBoxPlot :: (PointLike v n p, F.Foldable f)
              => f p -> BoxPlot v n
mkBoxPlot = mkBoxPlotOf folded

-- | Create a boxplot using a fold and given data.
mkBoxPlotOf :: PointLike v n p
            => Fold s p -> s -> BoxPlot v n
mkBoxPlotOf f a = GBoxPlot
  { bData = a
  , bFold = f . unpointLike
  , bPos  = id
  , bBox  = boxplotstat
  , bFill = True
  }

------------------------------------------------------------------------
-- Helper functions
------------------------------------------------------------------------

boxplotstat :: (Floating n, n ~ Double) => [P2 n] -> BP
boxplotstat ps = BP
   { bppoint = meanXY
   , bpw  = maxX * 0.5
   , bph1 = maxY * 0.3
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
makeRect  (BP (x,y) w h1 _h2) =
  fromVertices (map p2 [(xmin,y1max),(xmax,y1max),(xmax,y1min),(xmin,y1min)])
  where
    xmin  = x - w/2
    xmax  = x + w/2
    y1min = y - h1
    y1max = y + h1

----------------------------------------------------------------------------
-- Box plot lenses
----------------------------------------------------------------------------

class HasBox a v n d | a -> v n, a -> d where
  box :: Lens' a (GBoxPlot v n d)

  fillBox :: Lens' a Bool
  fillBox = box . lens bFill (\df fill -> df {bFill = fill})

instance HasBox (GBoxPlot v n d) v n d where
  box = id

instance HasBox (Plot (GBoxPlot v n d) b) v n d where
  box = rawPlot

------------------------------------------------------------------------
-- Boxplot
------------------------------------------------------------------------

-- $ boxplot
-- Box plots display data as boxplot. There are several representations
-- for boxplot plots for extra parameters. Box plots have the following
-- lenses:
--
-- @
-- * 'fillBox' :: 'Lens'' ('BoxPlot' v n) 'Bool' - False
-- @

-- | Add a 'BoxPlot' to the 'AxisState' from a data set.
--
-- @
--   myaxis = r2Axis &~
--     boxPlot data1
-- @
--
-- === __Example__
--
-- <<diagrams/src_Plots_Types_Boxplot_boxPlotExample.svg#diagram=boxPlotExample&width=300>>
--
-- > import Plots
-- > import Plots.Types.Boxplot
-- > mydata1 = [(1,3), (2,5.5), (3.2, 6), (3.5, 6.1)]
-- > mydata2 = mydata1 & each . _1 *~ 0.5
-- > mydata3 = [V2 1.2 2.7, V2 2 5.1, V2 3.2 2.6, V2 3.5 5]
--
-- > boxPlotAxis :: Axis B V2 Double
-- > boxPlotAxis = r2Axis &~ do
-- >    boxPlot mydata1 $ key "data 1"
-- >    boxPlot mydata2 $ key "data 2"
-- >    boxPlot mydata3 $ key "data 3"
--
-- > boxPlotExample = renderAxis boxPlotAxis
boxPlot
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (BoxPlot v n) b,
      F.Foldable f)
  => f p -> State (Plot (BoxPlot v n) b) () -> m ()
boxPlot d = addPlotable (mkBoxPlot d)

-- | Make a 'BoxPlot' and take a 'State' on the plot to alter its
--   options
--
-- === __Example__
--
-- <<diagrams/src_Plots_Types_Boxplot_boxPlotExample'.svg#diagram=boxPlotExample'&width=300>>
--
-- > import Plots
-- > import Plots.Types.Boxplot
-- > boxPlotAxis' :: Axis B V2 Double
-- > boxPlotAxis' = r2Axis &~ do
-- >    boxPlot' mydata1
-- >    boxPlot' mydata2
-- >    boxPlot' mydata3
--
-- > boxPlotExample' = renderAxis boxPlotAxis'
boxPlot'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (BoxPlot v n) b,
      F.Foldable f)
  => f p -> m ()
boxPlot' d = addPlotable' (mkBoxPlot d)

boxPlotOf
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (BoxPlot v n) b)
  => Fold s p -> s -> State (Plot (BoxPlot v n) b) () -> m ()
boxPlotOf f s = addPlotable (mkBoxPlotOf f s)

boxPlotOf'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (BoxPlot v n) b)
  => Fold s p -> s -> m ()
boxPlotOf' f s = addPlotable' (mkBoxPlotOf f s)

