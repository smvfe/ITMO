module HW3.T2
  ( distOption
  , distPair
  , distQuad
  , distAnnotated
  , distExcept
  , distPrioritised
  , distStream
  , distList
  , distFun
  , wrapOption
  , wrapPair
  , wrapQuad
  , wrapAnnotated
  , wrapExcept
  , wrapPrioritised
  , wrapStream
  , wrapList
  , wrapFun
  ) where

import HW3.T1

-- Option
distOption :: (Option a, Option b) -> Option (a, b)
distOption (Some a, Some b) = Some (a, b)
distOption _                = None

wrapOption :: a -> Option a
wrapOption = Some

-- Pair
distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair (P a1 a2, P b1 b2) = P (a1, b1) (a2, b2)

wrapPair :: a -> Pair a
wrapPair a = P a a

-- Quad
distQuad :: (Quad a, Quad b) -> Quad (a, b)
distQuad (Q a1 a2 a3 a4, Q b1 b2 b3 b4) = Q (a1, b1) (a2, b2) (a3, b3) (a4, b4)

wrapQuad :: a -> Quad a
wrapQuad a = Q a a a a

-- Annotated
distAnnotated :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated (a :# ea, b :# eb) = (a, b) :# (ea <> eb)

wrapAnnotated :: Monoid e => a -> Annotated e a
wrapAnnotated a = a :# mempty

-- Except
distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept (Success a, Success b) = Success (a, b)
distExcept (Error e, _)           = Error e
distExcept (_, Error e)           = Error e

wrapExcept :: a -> Except e a
wrapExcept = Success

-- Prioritised
distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised (High a,   High b)   = High   (a, b)
distPrioritised (High a,   Medium b) = High   (a, b)
distPrioritised (High a,   Low b)    = High   (a, b)
distPrioritised (Medium a, High b)   = High   (a, b)
distPrioritised (Medium a, Medium b) = Medium (a, b)
distPrioritised (Medium a, Low b)    = Medium (a, b)
distPrioritised (Low a,    High b)   = High   (a, b)
distPrioritised (Low a,    Medium b) = Medium (a, b)
distPrioritised (Low a,    Low b)    = Low    (a, b)

wrapPrioritised :: a -> Prioritised a
wrapPrioritised = Low

-- Stream
distStream :: (Stream a, Stream b) -> Stream (a, b)
distStream (a :> as, b :> bs) = (a, b) :> distStream (as, bs)

wrapStream :: a -> Stream a
wrapStream a = a :> wrapStream a

-- List
distList :: (List a, List b) -> List (a, b)
distList (Nil, _)       = Nil
distList (a :. as, bs)  = concatList (mapList (\b -> (a, b)) bs) (distList (as, bs))

concatList :: List a -> List a -> List a
concatList Nil       ys = ys
concatList (x :. xs) ys = x :. concatList xs ys

wrapList :: a -> List a
wrapList a = a :. Nil

-- Fun (apply and pair results)
distFun :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F fa, F fb) = F (\i -> (fa i, fb i))

wrapFun :: a -> Fun i a
wrapFun a = F (\_ -> a)
