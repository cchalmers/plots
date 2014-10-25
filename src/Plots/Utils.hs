{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Plots.Utils where

import Diagrams.Prelude hiding (diff)

class (V a ~ v, N a ~ n, Additive v, Num n) => InSpace v n a
instance (V a ~ v, N a ~ n, Additive v, Num n) => InSpace v n a

-- | @enumFromToN a b n@ calculates a list from a to b in n steps.
enumFromToN :: Fractional n => n -> n -> Int -> [n]
enumFromToN a b n = step n a
  where
    step i x | i < 1     = []
             | otherwise = step (i - 1) (x + diff)
    diff = (b - a) / fromIntegral n

