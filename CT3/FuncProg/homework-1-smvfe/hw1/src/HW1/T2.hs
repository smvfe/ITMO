module HW1.T2
  ( N (..)
  , nplus
  , nmult
  , nsub
  , nFromNatural
  , nToNum
  , ncmp
  , nEven
  , nOdd
  , ndiv
  , nmod
  ) where

import Numeric.Natural

data N = Z | S N

nplus :: N -> N -> N
nplus Z     n = n
nplus (S t) n = S (nplus t n)

nmult :: N -> N -> N
nmult Z     _ = Z
nmult _     Z = Z
nmult (S t) n = nplus n (nmult t n)

nsub :: N -> N -> Maybe N
nsub Z     Z     = Just Z
nsub Z     (S _) = Nothing
nsub n     Z     = Just n
nsub (S n) (S t) = nsub n t

ncmp :: N -> N -> Ordering
ncmp Z    Z      = EQ
ncmp Z    (S _)  = LT
ncmp (S _) Z     = GT
ncmp (S n) (S t) = ncmp n t

nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural n = S (nFromNatural (n - 1))

-- Uncomment this function signature to implement this function.
-- The correct signature is commented to avoid compilation failure
-- with '-Werror' in the template project.
nToNum :: Num a => N -> a
nToNum Z     = 0
nToNum (S n) = 1 + nToNum n

nEven :: N -> Bool
nEven Z     = True
nEven (S n) = not (nEven n)

nOdd :: N -> Bool
nOdd Z     = False
nOdd (S n) = not (nOdd n)

ndiv :: N -> N -> N
ndiv _ Z = error "division by zero"
ndiv n d =
  case nsub n d of
    Nothing -> Z
    Just r  -> S (ndiv r d)

nmod :: N -> N -> N
nmod _ Z = error "modulo by zero"
nmod n d =
  case nsub n d of
    Nothing -> n
    Just r  -> nmod r d
