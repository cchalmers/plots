{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}

module Plots.Name where

-- import Control.Lens
-- -- import Data.Map            (Map)
-- import Data.Ord            (comparing)
-- import Data.Function
-- import Data.Typeable
-- import Diagrams.Types.Names
-- import Diagrams.Prelude    hiding (view)

-- data PlotName = PlotName
--   { _plotName    :: String
--   , _namedSize2D :: SizeSpec V2 Double
--   , _namedT2     :: T2 Double
--   } deriving Typeable

-- makeLenses ''PlotName

-- instance Show PlotName where
--   show pn = "Plot: " ++ view plotName pn

-- -- equating :: Eq b => (a -> b) -> a -> a -> Bool
-- -- equating = on (==)

-- instance Eq PlotName where
--   (==) = on (==) (view plotName)

-- instance Ord PlotName where
--   compare = comparing (view plotName)

-- instance IsName PlotName

-- -- _AName :: IsName a => Prism' AName a
-- -- _AName = prism' AName (\(AName a) -> cast a)

-- -- _Names :: IsName a => Traversal' Name a
-- -- _Names = _Wrapped' . traverse . _AName

-- -- _NamedString :: Traversal' Name String
-- -- _NamedString = _Names

-- -- _NamedPlot :: Typeable n => Traversal' Name (PlotName n)
-- -- _NamedPlot = _Names

-- -- diaNames :: OrderedField n => QDiagram b V2 n Any -> Map Name [P2 n]
-- -- diaNames = over (mapped . traversed) location . view (subMap . _Wrapped')

