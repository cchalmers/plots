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

-- type instance V (ScatterPlot b v) = v

-- instance HasStyle (ScatterPlot b R2) where
--   applyStyle sty = over (themeEntry . themeMarker . recommend) (applyStyle sty)

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

-- instance (Typeable b, Typeable v, Renderable (Path R2) b)
--     => Plotable (ScatterPlot b v) b v where
--   plot _ l t = drawScatterPlot . over scatterPlotPoints (transform t . lmap l)

-- drawScatterPlot :: Renderable (Path R2) b => ScatterPlot b R2 -> Diagram b R2
-- drawScatterPlot sp = position (zip (sp^.scatterPlotPoints) (repeat mark))
--                   <> if sp ^. scatterPlotLines
--                        then fromVertices (sp ^. scatterPlotPoints)
--                               # applyStyle (sp ^. themeLineStyle)
--                        else mempty
--   where
--     mark = (sp ^. themeMarker)
--              # applyStyle (sp ^. themeMarkerStyle)

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


-- class ScatterPlotable a b v | a -> v where
--   mkScatterPlot :: a -> ScatterPlot b v
-- 
-- instance (CoordinateLike v a, Foldable f, Default (ScatterPlot b v)) => ScatterPlotable (f a) b v where
--   mkScatterPlot a =
--     def & scatterPlotPoints .~ points
--         & plotBoundingBox   .~ fromPoints points
--       where
--         points = a ^.. folded . diaCoord . _Unwrapped

-- instance (Renderable (Path R3) b, R3Like a, Foldable f) => ScatterPlotable (f a) b R3 where
--   mkScatterPlot a =
--     def & scatterPlotPoints .~ a ^.. folded . from r3Like . _Unwrapped



-- mkScatterPlot :: (Renderable (Path R2) b, Renderable Text b) => [P2] -> Bool -> String -> Plot b
-- mkScatterPlot points line legendName = review _ScatterPlot $
--   def & scatterPlotPoints .~ points
--       & scatterPlotLines  .~ line
--       & legendEntries .~ [scatterLegendEntry legendName]
--       & plotBoundingBox .~ fromPoints path
-- 
-- scatterLegendEntry :: (Renderable (Path R2) b, Renderable Text b) => String -> LegendEntry b
-- scatterLegendEntry txt = def & legendText .~ txt
--                              & legendPic  .~ scatterLegendPic
-- 
-- scatterLegendPic :: Renderable (Path R2) b => Bool -> ThemeEntry b -> Diagram b R2
-- scatterLegendPic line theme = (theme^.themeMarker._Just) # applyStyle (theme^.themeMarkerStyle)
--                            <> if line
--                                 then ((-1) ^& 0) ~~ (1 ^& 0) # applyStyle (theme^.themeLineStyle)
--                                 else mempty
