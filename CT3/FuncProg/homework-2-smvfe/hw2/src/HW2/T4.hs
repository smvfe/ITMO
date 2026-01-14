module HW2.T4
  ( ListPlus(..)
  , Inclusive(..)
  , DotString(..)
  , Fun(..)
  ) where

data ListPlus a = a :+ ListPlus a | Last a
  deriving (Eq, Show)
infixr 5 :+

instance Semigroup (ListPlus a) where
  Last i   <> ys  = i :+ ys
  (i :+ xs) <> ys = i :+ (xs <> ys)


data Inclusive a b = This a | That b | Both a b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
  This i    <> This j     = This (i <> j)
  This i    <> That j     = Both i j
  This i    <> Both i' j  = Both (i <> i') j

  That j    <> This i     = Both i j
  That j    <> That j'    = That (j <> j')
  That j    <> Both i j'  = Both i (j <> j')

  Both i j  <> This i'    = Both (i <> i') j
  Both i j  <> That j'    = Both i (j <> j')
  Both i j  <> Both i' j' = Both (i <> i') (j <> j')


newtype DotString = DS String
  deriving (Eq, Show)

instance Semigroup DotString where
  DS "" <> s      = s
  s      <> DS "" = s
  DS a   <> DS b  = DS (a ++ '.' : b)

instance Monoid DotString where
  mempty = DS ""


newtype Fun a = F (a -> a)

instance Semigroup (Fun a) where
  F f <> F g = F (f . g)

instance Monoid (Fun a) where
  mempty = F id
