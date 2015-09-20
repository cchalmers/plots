-- {-# LANGUAGE CPP                       #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TypeFamilies              #-}

module Plots.Types.Scatter
  ( -- * Scatter plot
    ScatterPlot
  , mkScatterPlot
  , mkScatterPlotOf
  -- , _ScatterPlot

    -- * Bubble plot
  , BubblePlot
  , mkBubblePlot
  , mkBubblePlotOf

    -- * General scatter plot
  , GScatterPlot
  , mkGScatterPlot
  , mkGScatterPlotOf

    -- * Scatter plot lenses
  , scatterTransform
  , scatterStyle
  , connectingLine

    -- ** Scatter plot
  , scatterPlot
  , scatterPlot'
  -- , scatterPlotL
    -- ** Fold variant scatter plot
  , scatterPlotOf
  , scatterPlotOf'
  -- , scatterPlotLOf

  , bubblePlot
  , bubblePlot'

    -- ** General scatter plot
  , gscatterPlot
  , gscatterPlot'
  -- , gscatterPlotL

  -- **fold variant
  --, gscatterPlotOf
  --, gscatterPlotOf'
  --, gscatterPlotLOf
  ) where

import           Control.Lens                    hiding (lmap, transform, ( # ))
import           Control.Monad.State.Lazy

import qualified Data.Foldable                   as F
import           Data.Typeable

import           Diagrams.Coordinates.Isomorphic
import           Diagrams.Prelude                hiding (view)

import           Plots.Axis
import           Plots.Style
import           Plots.Types

------------------------------------------------------------------------
-- General scatter plot
------------------------------------------------------------------------

-- | A general data type for scatter plots. Allows storing different
--   types of data as well as allowing transforms depending on the data.
data GScatterPlot v n a = forall s. GScatterPlot
  { sData :: s
  , sFold :: Fold s a
  , sPos  :: a -> Point v n
  , sTr   :: Maybe (a -> T2 n)
  , sSty  :: Maybe (a -> Style V2 n)
  , cLine :: Bool
  } deriving Typeable

type instance V (GScatterPlot v n a) = v
type instance N (GScatterPlot v n a) = n

instance (Metric v, OrderedField n) => Enveloped (GScatterPlot v n a) where
  getEnvelope GScatterPlot {..} = foldMapOf (sFold . to sPos) getEnvelope sData

instance (Typeable a, Typeable b, TypeableFloat n, Renderable (Path V2 n) b)
    => Plotable (GScatterPlot V2 n a) b where
  renderPlotable s _opts sty GScatterPlot {..} =
      foldMapOf sFold mk sData # applyMarkerStyle sty
   <> if cLine
        then fromVertices (toListOf (sFold . to sPos . to (logPoint ls)) sData)
               # transform t
               # applyLineStyle sty
        else mempty
    where
      t = s ^. specTrans
      ls = s ^. specScale
      mk a = marker # maybe id (transform  . ($ a)) sTr
                    # maybe id (applyStyle . ($ a)) sSty
                    # moveTo (specPoint s $ sPos a)
      marker = sty ^. plotMarker

  defLegendPic GScatterPlot {..} sty =
    sty ^. plotMarker
      & applyMarkerStyle sty

-- _ScatterPlot :: (Plotable (ScatterPlot v n) b, Typeable b)
--              => Prism' (Plot b v n) (ScatterPlot v n)
-- _ScatterPlot = _Plot

------------------------------------------------------------------------
-- Scatter plot
------------------------------------------------------------------------

-- | A plan scatter plot with no transformations depending on the data.
type ScatterPlot v n = GScatterPlot v n (Point v n)

-- | Make a scatter plot.
mkScatterPlot :: (PointLike v n p, F.Foldable f, Num n)
              => f p -> ScatterPlot v n
mkScatterPlot = mkScatterPlotOf folded

-- | Make a scatter plot using the given fold.
mkScatterPlotOf :: (PointLike v n p, Num n)
                => Fold s p -> s -> ScatterPlot v n
mkScatterPlotOf f a = GScatterPlot
  { sData = a
  , sFold = f . unpointLike
  , sPos  = id
  , sTr   = Nothing
  , sSty  = Nothing
  , cLine = False
  }

------------------------------------------------------------------------
-- Bubble plot
------------------------------------------------------------------------

-- | A scatter plot with a transform depending on an extra numerical
--   parameter.
type BubblePlot v n = GScatterPlot v n (n, Point v n)

mkBubblePlotOf :: (PointLike v n p, Fractional n)
               => Fold s (n,p) -> s -> BubblePlot v n
mkBubblePlotOf f a = GScatterPlot
  { sData = a
  , sFold = f . to (over _2 $ view unpointLike)
  , sPos  = snd
  , sTr   = Just (scaling . fst)
  , sSty  = Nothing
  , cLine = False
  }

mkBubblePlot :: (PointLike v n p, F.Foldable f, Fractional n)
             => f (n,p) -> BubblePlot v n
mkBubblePlot = mkBubblePlotOf folded

------------------------------------------------------------------------
-- General scatter plot
------------------------------------------------------------------------

mkGScatterPlotOf :: (PointLike v n p, Fractional n)
                 => Fold s a -> s -> (a -> p) -> GScatterPlot v n a
mkGScatterPlotOf f a pf = GScatterPlot
  { sData = a
  , sFold = f
  , sPos  = view unpointLike . pf
  , sTr   = Nothing
  , sSty  = Nothing
  , cLine = False
  }

mkGScatterPlot :: (PointLike v n p, F.Foldable f, Fractional n)
               => f a -> (a -> p) -> GScatterPlot v n a
mkGScatterPlot = mkGScatterPlotOf folded

------------------------------------------------------------------------
-- Scatter plot lenses
------------------------------------------------------------------------

class HasScatter a d | a -> d where
  scatter :: Lens' a (GScatterPlot (V a) (N a) d)

  -- | Lens onto the transform depending on the scatter plot data.
  scatterTransform :: Lens' a (Maybe (d -> T2 (N a)))
  scatterTransform = scatter . lens sTr (\sp t -> sp {sTr = t})

  -- | Change the style for a scatter plot, given the data entry.
  --
  -- @
  -- mybubbleplot & scatterStyle     ?~ mkAttr . transparency
  --              & scatterTransform .~ Nothing
  -- @
  --
  scatterStyle :: Lens' a (Maybe (d -> Style V2 (N a)))
  scatterStyle = scatter . lens sSty (\sp sty -> sp {sSty = sty})

  -- | Lens onto whether the scatter plot should have a connecting line
  --   between points.
  connectingLine :: Lens' a Bool
  connectingLine = scatter . lens cLine (\s b -> (s {cLine = b}))

instance HasScatter (GScatterPlot v n d) d where
  scatter = id

-- instance HasScatter p d => HasScatter (Plot p b) d where
--   scatter = rawPlot . scatter

------------------------------------------------------------------------
-- Scatter plot
------------------------------------------------------------------------

-- $ scatter
-- Scatter plots display data as dots. There are several representations
-- for scatter plots for extra parameters. Scatter plots have the
-- following lenses:
--
-- @
-- * 'connectingLine' :: 'Lens'' ('ScatterPlot' v n) 'Bool' - False
-- * 'scatterTransform' :: 'Lens'' ('ScatterPlot' v n) ('Maybe' ('Point' v n -> 'T2' n)) - Nothing
-- * 'scatterStyle': 'Maybe' ('Point' v n -> 'Style' 'V2' n) - Nothing
-- @
--

-- | Add a 'ScatterPlot' to the 'AxisState' from a data set.
--
-- @
--   myaxis = r2Axis ~&
--     scatterPlot data1
-- @
--
-- === __Example__
--
-- <<plots/scatter.png#diagram=scatter&width=300>>
--
-- @
-- mydata1 = [(1,3), (2,5.5), (3.2, 6), (3.5, 6.1)]
-- mydata2 = mydata1 & each . _1 *~ 0.5
-- mydata3 = [V2 1.2 2.7, V2 2 5.1, V2 3.2 2.6, V2 3.5 5]
--
-- myaxis :: Axis B V2 Double
-- myaxis = r2Axis &~ do
--   scatterPlotL "data 1" mydata1
--   scatterPlotL "data 2" mydata2
--   scatterPlotL "data 3" mydata3
-- @

scatterPlot
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (ScatterPlot v n) b,
      Typeable b,
      F.Foldable f)
  => f p -> State (Plot (ScatterPlot v n) b) () -> m ()
scatterPlot d = addPlotable (mkScatterPlot d)

-- | Make a 'ScatterPlot' and take a 'State' on the plot to alter it's
--   options
--
-- @
--   myaxis = r2Axis &~ do
--     scatterPlot' pointData1 $ do
--       connectingLine .= True
--       addLegendEntry "data 1"
-- @

scatterPlot'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (ScatterPlot v n) b,
      Typeable b,
      F.Foldable f)
  => f p -> m ()
scatterPlot' d = addPlotable' (mkScatterPlot d)

-- | Add a 'ScatterPlot' with the given name for the legend entry.
--
-- @
--   myaxis = r2Axis &~ do
--     scatterPlotL "blue team" pointData1
--     scatterPlotL "red team" pointData2
-- @
--
-- === __Example__
--
-- <<plots/scatter-2.png#diagram=scatter&width=300>>
--
-- @
-- mydata1 = [(1,3), (2,5.5), (3.2, 6), (3.5, 6.1)]
-- mydata2 = mydata1 & each . _1 *~ 0.5
-- mydata3 = [V2 1.2 2.7, V2 2 5.1, V2 3.2 2.6, V2 3.5 5]
--
-- myaxis :: Axis B V2 Double
-- myaxis = r2Axis &~ do
--    scatterPlot' mydata1 $ do
--      addLegendEntry "data 1"
--      plotColor .= purple
--      plotMarker %= scale 2
--    scatterPlotL "data 2" mydata2
--    scatterPlotL "data 3" mydata3
-- @

-- scatterPlotL
--   :: (v ~ BaseSpace c,
--       PointLike v n p,
--       MonadState (Axis b c n) m,
--       Plotable (ScatterPlot v n) b,
--       F.Foldable f)
--   => String -> f p -> m ()
-- scatterPlotL l d = addPlotableL l (mkScatterPlot d)

-- Fold variants

scatterPlotOf
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (ScatterPlot v n) b)
  => Fold s p -> s -> State (Plot (ScatterPlot v n) b) () -> m ()
scatterPlotOf f s = addPlotable (mkScatterPlotOf f s)

scatterPlotOf'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (ScatterPlot v n) b)
  => Fold s p -> s -> m ()
scatterPlotOf' f s = addPlotable' (mkScatterPlotOf f s)

-- scatterPlotLOf
--   :: (v ~ BaseSpace c,
--       PointLike v n p,
--       MonadState (Axis b c n) m,
--       Plotable (ScatterPlot v n) b)
--   => String -> Fold s p -> s -> m ()
-- scatterPlotLOf l f s = addPlotableL l (mkScatterPlotOf f s)

------------------------------------------------------------------------
-- Bubble plot --
------------------------------------------------------------------------

-- $ bubble
-- Scatter plots with extra numeric parameter. By default the extra
-- parameter is the scale of the marker but this can be changed.

bubblePlot :: (v ~ BaseSpace c, Fractional n, Foldable f, PointLike v n p,
               MonadState (Axis b c n) m,
               Plotable (BubblePlot v n) b) =>
              f (n, p) -> State (Plot (BubblePlot v n) b) () -> m ()
bubblePlot d = addPlotable (mkBubblePlot d)

bubblePlot' :: (v ~ BaseSpace c, Fractional n, Foldable f, PointLike v n p,
                MonadState (Axis b c n) m,
                Plotable (BubblePlot v n) b) =>
               f (n, p) -> m ()
bubblePlot' d = addPlotable' (mkBubblePlot d)

------------------------------------------------------------------------
-- General scatter plot
------------------------------------------------------------------------

gscatterPlot
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (GScatterPlot v n a) b,
      F.Foldable f)
  => f a -> (a -> p) -> State (Plot (GScatterPlot v n a) b) () -> m ()
gscatterPlot d pf = addPlotable (mkGScatterPlot d pf)

gscatterPlot'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (GScatterPlot v n a) b,
      F.Foldable f)
  => f a -> (a -> p) -> m ()
gscatterPlot' d pf = addPlotable' (mkGScatterPlot d pf)

-- gscatterPlotL
--   :: (v ~ BaseSpace c,
--       PointLike v n p,
--       MonadState (Axis b c n) m,
--       Plotable (GScatterPlot v n a) b,
--       F.Foldable f)
--   => String -> f a -> (a -> p) -> m ()
-- gscatterPlotL l d pf = addPlotableL l (mkGScatterPlot d pf)
