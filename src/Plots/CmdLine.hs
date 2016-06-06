{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- Orphan Mainable Axis instance. It needs to be an orphan anyway because 'RenderAxis'
-- can use 'renderAxis'.
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Plots.CmdLine
  ( r2AxisMain
  ) where

import           Diagrams.Backend.CmdLine
import           Diagrams.Prelude         hiding (option, (<>))
import           Plots.Axis
import           Plots.Axis.Render

-- In the future we may want to have extra command line options like
-- this:

-- data PlotOptions = PlotOptions
--   { plotTitle :: Maybe String
--   }

-- instance Parseable PlotOptions where
--   parser = PlotOptions
--     <$> (optional . strOption)
--         (long "title" <> short 't' <> help "Plot title")

--   mainRender (opts, pOpts) s
--     = mainRender opts . renderAxis
--     $ a & maybe id (set axisTitle) (plotTitle pOpts)

instance (TypeableFloat n,
          Renderable (Path V2 n) b,
          Mainable (QDiagram b V2 n Any))
       => Mainable (Axis b V2 n) where
  type MainOpts (Axis b V2 n) = MainOpts (QDiagram b V2 n Any)

  mainRender opts = mainRender opts . renderAxis

instance ToResult (Axis b v n) where
  type Args (Axis b v n) = ()
  type ResultOf (Axis b v n) = Axis b v n

  toResult d _ = d

-- | 'mainWith' specialised to a 2D Axis.
r2AxisMain :: (Parseable (MainOpts (QDiagram b V2 Double Any)), Mainable (Axis b V2 Double)) => Axis b V2 Double -> IO ()
r2AxisMain = mainWith
