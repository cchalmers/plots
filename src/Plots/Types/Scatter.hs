{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}


module Plots.Types.Scatter
  ( ScatterPlot
    -- * Prism
  , _ScatterPlot
    -- * Lenses
  , scatterPlotPoints
  , scatterPlotLines
  ) where

import Control.Lens     hiding (transform, ( # ))
import Data.Default
import Data.Typeable
import Diagrams.Prelude
import Diagrams.BoundingBox
-- import Diagrams.TwoD.Text

import Plots.Types
import Plots.Themes

data ScatterPlot b v = ScatterPlot
  { _scatterPlotPoints  :: [Point v]
  , _scatterPlotLines   :: Bool
  , _scatterGenericPlot :: GenericPlot b v
  } deriving Typeable

makeLenses ''ScatterPlot

type instance V (ScatterPlot b v) = v

instance HasStyle (ScatterPlot b R2) where
  applyStyle sty = over (genericPlot . themeMarker) (applyStyle sty)

instance Renderable (Path R2) b => Default (ScatterPlot b R2) where
  def = ScatterPlot
          { _scatterPlotPoints  = []
          , _scatterPlotLines   = False
          , _scatterGenericPlot = def
          }

instance HasGenericPlot (ScatterPlot b v) b v where
  genericPlot = scatterGenericPlot

instance HasLinearMap v => Transformable (ScatterPlot b v) where
  transform t = over scatterPlotPoints (transform t)

_ScatterPlot :: Plotable (ScatterPlot b v) b v => Prism' (Plot b v) (ScatterPlot b v)
_ScatterPlot = _Plot

instance (Typeable b, Renderable (Path R2) b) => Plotable (ScatterPlot b R2) b R2 where
  plot _ _ t = drawScatterPlot . over scatterPlotPoints (transform t)

drawScatterPlot :: Renderable (Path R2) b => ScatterPlot b R2 -> Diagram b R2
drawScatterPlot sp = position (zip (sp^.scatterPlotPoints) (repeat mark))
                  <> if sp^.scatterPlotLines
                       then fromVertices (sp^.scatterPlotPoints)
                              # applyStyle (sp ^. genericPlot . themeLineStyle)
                       else mempty
  where
    mark = (sp^. genericPlot . themeMarker)
             # applyStyle (sp ^. genericPlot . themeMarkerStyle)

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
