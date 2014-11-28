{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE FunctionalDependencies #-}


module Plots.Types.Scatter where
--   ( ScatterPlot
--   , mkScatterPlot
--     -- * Prism
--   , _ScatterPlot
--     -- * Lenses
--   , scatterPlotPoints
--   , scatterPlotLines
--   ) where
-- 
-- import Control.Lens     hiding (transform, ( # ), lmap)
-- import Diagrams.LinearMap
-- import Data.Default
-- import Data.Typeable
-- import Diagrams.Prelude
-- import Diagrams.BoundingBox
-- import Data.Foldable
-- 
-- import Diagrams.Coordinates.Isomorphic
-- -- import Diagrams.TwoD.Text
-- 
-- import Plots.Types
-- import Plots.Themes
-- 
-- -- | Data type for holding scatter plot information. 
-- data ScatterPlot b v n = ScatterPlot
--   { _scatterPlotPoints  :: [Point v n]
--   , _scatterPlotLines   :: Bool
--   , _scatterGenericPlot :: GenericPlot b v n
--   } deriving Typeable
-- 
-- makeLenses ''ScatterPlot
-- 
-- type instance V (ScatterPlot b v n) = v
-- type instance N (ScatterPlot b v n) = n
-- type instance B (ScatterPlot b v n) = b
-- 
-- instance (Renderable (Path V2 n) b, Additive v, TypeableFloat n)
--     => Default (ScatterPlot b v n) where
--   def = ScatterPlot
--           { _scatterPlotPoints  = []
--           , _scatterPlotLines   = False
--           , _scatterGenericPlot = def
--           }
-- 
-- instance HasGenericPlot (ScatterPlot b v n) where
--   genericPlot = scatterGenericPlot
-- 
-- instance (Typeable b, Typeable v, TypeableFloat n, Renderable (Path V2 n) b, HasLinearMap v)
--     => Plotable (ScatterPlot b v n) where
--   plot _ tv l t2 sp = position (zip points (repeat mark))
--                    <> line
--     where
--       mark   = applyStyle (sp ^. themeMarkerStyle) (sp ^. themeMarker)
--       points = transform t2 . lmap l . transform tv $ sp ^. scatterPlotPoints
--       --
--       line | sp ^. scatterPlotLines = fromVertices points
--                                         # applyStyle (sp ^. themeLineStyle)
--            | otherwise              = mempty
-- 
--   defLegendPic a = mkMark a <> line
--     where
--       line | a ^. scatterPlotLines = (p2 (-10,0) ~~ p2 (10,0))
--                                        # applyStyle (a ^. themeLineStyle)
--            | otherwise             = mempty
-- 
-- mkMark :: (HasThemeEntry a b n, TypeableFloat n, HasGenericPlot a) => a -> QDiagram b V2 n Any
-- mkMark a = (a ^. themeMarker) # applyStyle (a ^. themeMarkerStyle)
-- 
-- -- | Prism specific to a 'ScatterPlot'.
-- _ScatterPlot :: Plotable (ScatterPlot b v n) => Prism' (Plot b v n) (ScatterPlot b v n)
-- _ScatterPlot = _Plot
-- 
-- 
-- -- | Standard way to make a scatter plot. 
-- mkScatterPlot
--   :: (PointLike v n a, Ord n, Foldable f, Default (ScatterPlot b v n))
--   => f a -> ScatterPlot b v n
-- mkScatterPlot a =
--     def & scatterPlotPoints .~ points
--         & plotBoundingBox   .~ fromPoints points
--       where
--         points = a ^.. folded . re pointLike
-- 
