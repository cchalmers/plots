{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Compatability layer between diagrams, lens and linear with lots of extras.

module Diagrams.Prelude2
  ( module Exports
    -- * Transformations with matricies.
  -- new classes

  ) where

import Control.Lens                    as Exports hiding (at, backwards,
                                                   none, transform,
                                                   ( # ), (.>), (<.>),
                                                   (|>), lmap)
import Data.Default                    as Exports
import Data.Distributive               as Exports
import Data.Foldable                   as Exports
import Data.Traversable                as Exports
import Diagrams.Coordinates            as Exports
import Diagrams.Core                   as Exports
import Diagrams.Core.Transform         as Exports
import Diagrams.Core.Types             as Exports
import Diagrams.Extra                  as Exports
import Data.Typeable                   as Exports
import Diagrams.TwoD.Text              as Exports
import Diagrams.Prelude                as Exports hiding (beside, under,
                                                   view, unitX, unit_X, unitY, 
                                                   unit_Y, scalingX, scalingY,
                                                   reflectionX, reflectionY, 
                                                   frame)
import Linear                          as Exports hiding (Conjugate, R1,
                                                   R2, R3, Trace, basis,
                                                   conjugate, distance,
                                                   lerp, rotate, sumV,
                                                   trace, translation,
                                                   (*^), (^*), (^+^),
                                                   (^-^), (^/), _x, _y,
                                                   _z, _xy)

