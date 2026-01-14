module HW3.T3
  ( joinOption
  , joinExcept
  , joinAnnotated
  , joinList
  , joinFun
  ) where

import HW3.T1

-- Option
joinOption :: Option (Option a) -> Option a
joinOption None         = None
joinOption (Some None)  = None
joinOption (Some (Some a)) = Some a

-- Except
joinExcept :: Except e (Except e a) -> Except e a
joinExcept (Error e)           = Error e
joinExcept (Success (Error e)) = Error e
joinExcept (Success (Success a)) = Success a

-- Annotated
joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((a :# inner) :# outer) = a :# (outer <> inner)

-- List
joinList :: List (List a) -> List a
joinList Nil          = Nil
joinList (xs :. xss)  = concatList xs (joinList xss)

concatList :: List a -> List a -> List a
concatList Nil       ys = ys
concatList (x :. xs) ys = x :. concatList xs ys

-- Fun
joinFun :: Fun i (Fun i a) -> Fun i a
joinFun (F outer) = F (\i -> let F inner = outer i in inner i)
