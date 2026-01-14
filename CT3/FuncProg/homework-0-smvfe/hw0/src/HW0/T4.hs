module HW0.T4
  ( repeat'
  , map'
  , fib
  , fac
  ) where

import Data.Function (fix)
import Numeric.Natural (Natural)

repeat' :: a -> [a]
repeat' x = fix (x :)

map' :: (a -> b) -> [a] -> [b]
map' f = fix (\rec xs -> case xs of
  []     -> []
  (y:ys) -> f y : rec ys)

fib :: Natural -> Natural
fib n = fix (\fibs -> 0 : 1 : zipWith (+) fibs (tail fibs)) `genericIndex` n
  where
    genericIndex (x:_)  0 = x
    genericIndex (_:xs) k = genericIndex xs (k - 1)
    genericIndex []     _ = error "index out of bounds"

fac :: Natural -> Natural
fac = fix (\rec n -> case n of
  0 -> 1
  _ -> n * rec (n - 1))
