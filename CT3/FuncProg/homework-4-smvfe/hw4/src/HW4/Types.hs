module HW4.Types
  ( Annotated(..)
  , Except(..)
  , Prim(..)
  , Expr(..)
  , mapAnnotated
  , mapExcept
  ) where

data Annotated e a = a :# e
  deriving Show

infix 0 :#

mapAnnotated :: (a -> b) -> (Annotated e a -> Annotated e b)
mapAnnotated f (a :# e) = f a :# e

data Except e a = Error e | Success a
  deriving Show

mapExcept :: (a -> b) -> (Except e a -> Except e b)
mapExcept _ (Error e)   = Error e
mapExcept f (Success a) = Success (f a)

data Prim a
  = Add a a
  | Sub a a
  | Mul a a
  | Div a a
  | Abs a
  | Sgn a
  deriving Show

data Expr = Val Double | Op (Prim Expr)
  deriving Show

instance Num Expr where
  x + y         = Op (Add x y)
  x - y         = Op (Sub x y)
  x * y         = Op (Mul x y)
  abs x         = Op (Abs x)
  signum x      = Op (Sgn x)
  fromInteger n = Val (fromInteger n)

instance Fractional Expr where
  x / y          = Op (Div x y)
  fromRational r = Val (fromRational r)
