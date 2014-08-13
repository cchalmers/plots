{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE FunctionalDependencies #-}


module Plots.Types.Scatter
  ( ScatterPlot
  , mkScatterPlot
    -- * Prism
  , _ScatterPlot
    -- * Lenses
  , scatterPlotPoints
  , scatterPlotLines
  ) where

import Control.Lens     hiding (transform, ( # ), lmap)
import Diagrams.LinearMap
import Data.Default
import Data.Typeable
import Diagrams.Prelude
import Diagrams.BoundingBox
import Data.Foldable
import Diagrams.Coordinates.Isomorphic
import Diagrams.Coordinates.Traversals
-- import Diagrams.TwoD.Text

import Plots.Types
import Plots.Themes

-- | Data type for holding scatter plot information. 
data ScatterPlot b v = ScatterPlot
  { _scatterPlotPoints  :: [Point v]
  , _scatterPlotLines   :: Bool
  , _scatterGenericPlot :: GenericPlot b v
  } deriving Typeable

makeLenses ''ScatterPlot

type instance V (ScatterPlot b v) = v

instance (Renderable (Path R2) b, HasLinearMap v, Applicative (T v))
    => Default (ScatterPlot b v) where
  def = ScatterPlot
          { _scatterPlotPoints  = []
          , _scatterPlotLines   = False
          , _scatterGenericPlot = def
          }

instance HasGenericPlot (ScatterPlot b v) b v where
  genericPlot = scatterGenericPlot

instance (Typeable b, Typeable v, Renderable (Path R2) b, Scalar v ~ Double, HasLinearMap v)
    => Plotable (ScatterPlot b v) b v where
  plot _ tv l t2 sp = position (zip points (repeat mark))
               <> line
    where
      mark   = applyStyle (sp ^. themeMarkerStyle) (sp ^. themeMarker)
      points = transform t2 . lmap l . transform tv $ sp ^. scatterPlotPoints
      --
      line | sp ^. scatterPlotLines = fromVertices points
                                        # applyStyle (sp ^. themeLineStyle)
           | otherwise              = mempty

-- | Prism specific to a 'ScatterPlot'.
_ScatterPlot :: Plotable (ScatterPlot b v) b v => Prism' (Plot b v) (ScatterPlot b v)
_ScatterPlot = _Plot


-- | Standard way to make a scatter plot. 
mkScatterPlot
  :: (PointLike a v, Foldable f, Default (ScatterPlot b v))
  => f a -> ScatterPlot b v
mkScatterPlot a =
    def & scatterPlotPoints .~ points
        & plotBoundingBox   .~ fromPoints points
      where
        points = a ^.. folded . diaPoint

