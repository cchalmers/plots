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

{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE ConstraintKinds           #-}

{-# LANGUAGE StandaloneDeriving        #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Plots.Types.Line
  ( -- * Trail plot
    mkTrail
  , mkTrailOf

    -- * Path plot
  , mkPath
  , mkPathOf

    -- * GLinePlot plot
  , GLinePlot
  , _LinePlot
  , mkGLinePlotOf
  , mkGLinePlot

    -- * Line plot
  , LinePlot
  , mkLinePlotOf
  , mkLinePlot

    -- * Helper functions
  , createStepData

    -- * Lenses
  , dotsonPoint
  , pathStyle

    -- * Line Plot
  , linePlot
  , linePlot'
  , linePlot''
  , linePlotL
  , linePlotOf
  , linePlotOf'
  , linePlotLOf

  , pathPlot''

  --, createstep
    -- * Step plot
  , stepPlot
  , stepPlot'
  , stepPlotL

    -- * Gline plot
  , glinePlot
  , glinePlot'
  , glinePlotL
  ) where

import           Control.Lens     hiding (transform, ( # ), lmap)
import           Control.Monad.State.Lazy

import qualified Data.Foldable    as F
import           Data.Typeable

import           Diagrams.Coordinates.Isomorphic
import           Diagrams.Prelude  hiding (view)

import           Plots.Style
import           Plots.Types
import           Plots.API

------------------------------------------------------------------------
-- Trail and path
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
-- GLine plot
------------------------------------------------------------------------

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
-- Helper functions
------------------------------------------------------------------------

-- | Create a data for step plots.
createStepData :: [(a,a)] -> [(a,a)]
createStepData [] = []
createStepData (x1:[]) = x1:[]
createStepData (x1:x2:xs) = (x1):(fst x1, snd x2):(x2):(createStepData (x2:xs))


-- type StepPlot v n = GLinePlot v n (Point v n)

-- mkStepPlotOf :: (PointLike v n p, Fractional n)
--                => Fold s p -> s -> StepPlot v n
-- mkStepPlotOf f a = GLinePlot
--   { sData = a
--   , sFold = f . unpointLike
--   , sPos  = id
--   , sSty  = Nothing
--   , cPnt  = False
--   }

-- mkStepPlot :: (PointLike v n p, F.Foldable f, Fractional n)
--              => f p -> StepPlot v n
-- mkStepPlot = mkStepPlotOf folded

------------------------------------------------------------------------
-- General Line Plot
------------------------------------------------------------------------

-- | Plot a general line plot.
mkGLinePlot :: (PointLike v n p, F.Foldable f, Fractional n)
               => f a -> (a -> p) -> GLinePlot v n a
mkGLinePlot = mkGLinePlotOf folded

-- | Plot a general line plot give a fold
mkGLinePlotOf :: (PointLike v n p, Fractional n)
                 => Fold s a -> s -> (a -> p) -> GLinePlot v n a
mkGLinePlotOf f a pf = GLinePlot
  { sData = a
  , sFold = f
  , sPos  = view unpointLike . pf
  , sSty  = Nothing
  , cPnt = False
  }


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

------------------------------------------------------------------------
-- Line plot
------------------------------------------------------------------------

-- $ line
-- line plots display data as dots. There are several representations
-- for line plots for extra parameters. Line plots have the
-- following lenses:
--
-- @
-- * 'dotsonPoint' :: 'Lens'' ('LinePlot' v n) 'Bool' - False
-- * 'lineStyle'   :: 'Maybe' ('Point' v n -> 'Style' 'V2' n) - Nothing
-- @
--
-- | Add a 'LinePlot' to the 'AxisState' from a data set.
--
-- @
--   myaxis = r2Axis ~&
--     linePlot data1
-- @
-- === __Example__
--
-- <<plots/line.png#diagram=line&width=300>>
--
-- @
--
-- myaxis :: Axis B V2 Double
-- myaxis = r2Axis &~ do
--          linePlot  mydata1
--          linePlot' mydata2 $ do
--               addLegendEntry "data 2"
--               plotColor .= black
--               dotsonPoint .= False
--          linePlotL "data 3" mydata3
--
--          axisPlots . each . _LinePlot' . dotsonPoint .= True
--
-- @

linePlot
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (LinePlot v n) b,
      F.Foldable f)
  => f p -> m ()
linePlot d = addPlotable (mkLinePlot d)

-- | Make a 'LinePlot' and take a 'State' on the plot to alter it's
--   options
--
linePlot'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (LinePlot v n) b,
      F.Foldable f)
  => f p -> PlotState (LinePlot v n) b -> m ()
linePlot' d = addPlotable' (mkLinePlot d)

-- | Add a 'LinePlot' with the given name for the legend entry.
--
-- @
--   myaxis = r2Axis &~ do
--     linePlotL "blue team" pointData1
--     linePlotL "red team" pointData2
-- @
linePlotL
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (LinePlot v n) b,
      F.Foldable f)
  => String -> f p -> m ()
linePlotL l d = addPlotableL l (mkLinePlot d)


-- | mkTrail version of line plot

linePlot''
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      R2Backend b n,
      Plotable (Path v n) b,
      F.Foldable f)
  => f p -> m ()
linePlot'' d = addPlotable (mkPath $ Identity d)

pathPlot'' :: (R2Backend b n, MonadState (Axis b V2 n) m) => Path V2 n -> m ()
pathPlot'' = addPlotable

-- Fold variants

linePlotOf
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (LinePlot v n) b)
  => Fold s p -> s -> m ()
linePlotOf f s = addPlotable (mkLinePlotOf f s)

linePlotOf'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (LinePlot v n) b)
  => Fold s p -> s -> PlotState (LinePlot v n) b -> m ()
linePlotOf' f s = addPlotable' (mkLinePlotOf f s)

linePlotLOf
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (LinePlot v n) b)
  => String -> Fold s p -> s -> m ()
linePlotLOf l f s = addPlotableL l (mkLinePlotOf f s)

------------------------------------------------------------------------
--Step
------------------------------------------------------------------------

stepPlot :: (RealFloat n, Typeable n, Typeable b, Renderable (Path V2 n) b,
             MonadState (Axis b c n) m, BaseSpace c ~ V2)
         => [(n, n)] -> m ()
stepPlot  a   = linePlot (createStepData a)

stepPlot' :: (RealFloat n, Typeable n, Typeable b, Renderable (Path V2 n) b,
              MonadState (Axis b c n) m, BaseSpace c ~ V2) =>
             [(n, n)] -> PlotState (LinePlot V2 n) b -> m ()
stepPlot' a   = linePlot' (createStepData a)

stepPlotL :: (RealFloat n, Typeable n, Typeable b, Renderable (Path V2 n) b,
              MonadState (Axis b c n) m, BaseSpace c ~ V2) =>
             String -> [(n, n)] -> m ()
stepPlotL l a = linePlotL l (createStepData a)

------------------------------------------------------------------------
-- General Line Plot
------------------------------------------------------------------------


glinePlot
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (GLinePlot v n a) b,
      F.Foldable f)
  => f a -> (a -> p) -> m ()
glinePlot d pf = addPlotable (mkGLinePlot d pf)

glinePlot'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (GLinePlot v n a) b,
      F.Foldable f)
  => f a -> (a -> p) -> PlotState (GLinePlot v n a) b -> m ()
glinePlot' d pf = addPlotable' (mkGLinePlot d pf)

glinePlotL
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (GLinePlot v n a) b,
      F.Foldable f)
  => String -> f a -> (a -> p) -> m ()
glinePlotL l d pf = addPlotableL l (mkGLinePlot d pf)

