{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Plots.CmdLine where
  -- ( module Plots
  -- , r2AxisMain
  -- ) where

-- import Data.Typeable
-- import Options.Applicative

-- -- import Diagrams.Prelude hiding ((<>), option)
-- import Diagrams.Backend.CmdLine
-- import Diagrams.Prelude hiding (option, (<>))
-- import Diagrams.TwoD.Text       (Text)
-- import Plots.Axis
-- import Plots

-- data PlotOptions = PlotOptions
  -- { plotTitle :: Maybe String
  -- }

-- instance Parseable PlotOptions where
  -- parser = PlotOptions
  --   <$> (optional . strOption)
  --       (long "title" <> short 't' <> help "Plot title")


-- instance (Typeable b,
  --         TypeableFloat n,
  --         Renderable (Text n) b,
  --         Renderable (Path V2 n) b,
  --         Backend b V2 n,
  --         Mainable (QDiagram b V2 n Any))
  --      => Mainable (Axis b V2 n) where
  -- type MainOpts (Axis b V2 n) = (MainOpts (QDiagram b V2 n Any), PlotOptions)

  -- mainRender (opts, pOpts) a
  --   = mainRender opts . renderAxis
  --   $ a & axisTitle .~ plotTitle pOpts

-- --
-- r2AxisMain :: (Parseable (MainOpts (QDiagram b V2 Double Any)), Mainable (Axis b V2 Double)) => Axis b V2 Double -> IO ()
-- r2AxisMain = mainWith
