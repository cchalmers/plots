{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Plots.CmdLine
  ( module Plots
	, r2AxisMain
	) where

import Data.Typeable
import Options.Applicative hiding ((&))

-- import Diagrams.Prelude hiding ((<>), option)
import Diagrams.Backend.CmdLine
import Diagrams.Prelude         (Backend, Diagram, Path, R2, Renderable, (&), (.~))
import Diagrams.TwoD.Text       (Text)
import Plots.Axis
import Plots

data PlotOptions = PlotOptions
  { plotTitle :: Maybe String
  }

instance Parseable PlotOptions where
  parser = PlotOptions
    <$> (optional . option)
        (long "title" <> short 't' <> help "Plot title")


instance (Typeable b,
          Renderable Text b,
          Renderable (Path R2) b,
          Backend b R2,
          Mainable (Diagram b R2))
       => Mainable (Axis b R2) where
  type MainOpts (Axis b R2) = (MainOpts (Diagram b R2), PlotOptions)

  mainRender (opts, pOpts) a = mainRender opts . renderR2Axis $ a & axisTitle .~ plotTitle pOpts

r2AxisMain :: (Parseable (MainOpts (Diagram b R2)), Mainable (Axis b R2)) => Axis b R2 -> IO ()
r2AxisMain = mainWith
