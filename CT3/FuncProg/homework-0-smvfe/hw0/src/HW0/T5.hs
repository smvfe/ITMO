module HW0.T5
  ( Nat
  , nz
  , ns
  , nplus
  , nmult
  , nFromNatural
  , nToNum
  ) where

import Numeric.Natural (Natural)

type Nat a = (a -> a) -> a -> a

nz :: Nat a
nz _ z = z

ns :: Nat a -> Nat a
ns n f z = f (n f z)

nplus :: Nat a -> Nat a -> Nat a
nplus a b f z = a f (b f z)

nmult :: Nat a -> Nat a -> Nat a
nmult a b f = a (b f)

nFromNatural :: Natural -> Nat a
nFromNatural 0 = nz
nFromNatural n = ns (nFromNatural (n - 1))

nToNum :: Num a => Nat a -> a
nToNum n = n (+ 1) 0
