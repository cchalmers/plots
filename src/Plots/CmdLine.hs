{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- Orphan Mainable Axis instance. It needs to be an orphan anyway because 'RenderAxis'
-- can use 'renderAxis'.
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Plots.CmdLine
  ( r2AxisMain
  ) where

import Data.Typeable
-- import Options.Applicative

import Diagrams.Backend.CmdLine
import Diagrams.Prelude hiding (option, (<>))
import Diagrams.TwoD.Text       (Text)
import Plots.Axis
import Plots.Axis.Render

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

instance (Typeable b,
          TypeableFloat n,
          Renderable (Text n) b,
          Renderable (Path V2 n) b,
          Backend b V2 n,
          Mainable (QDiagram b V2 n Any))
       => Mainable (Axis b V2 n) where
  type MainOpts (Axis b V2 n) = MainOpts (QDiagram b V2 n Any)

  mainRender opts = mainRender opts . renderAxis

-- | 'mainWith' specialised to a 2D Axis.
r2AxisMain :: (Parseable (MainOpts (QDiagram b V2 Double Any)), Mainable (Axis b V2 Double)) => Axis b V2 Double -> IO ()
r2AxisMain = mainWith
