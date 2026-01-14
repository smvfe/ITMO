module HW3.T4
  ( State(..)
  , mapState
  , wrapState
  , joinState
  , modifyState
  , Prim(..)
  , Expr(..)
  , eval
  ) where

import HW3.T1 (Annotated(..), mapAnnotated)

data State s a = S { runS :: s -> Annotated s a }

mapState :: (a -> b) -> State s a -> State s b
mapState f (S run) = S (\s -> mapAnnotated f (run s))

wrapState :: a -> State s a
wrapState a = S (\s -> a :# s)

joinState :: State s (State s a) -> State s a
joinState (S outer) = S (\s ->
  let (S inner) :# s' = outer s
  in inner s')

modifyState :: (s -> s) -> State s ()
modifyState f = S (\s -> () :# f s)

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  sf <*> sa = S (\s ->
    let f :# s'  = runS sf s
        a :# s'' = runS sa s'
    in (f a) :# s'')

instance Monad (State s) where
  sa >>= f = joinState (mapState f sa)

data Prim a
  = Add a a
  | Sub a a
  | Mul a a
  | Div a a
  | Abs a
  | Sgn a

data Expr = Val Double | Op (Prim Expr)

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

eval :: Expr -> State [Prim Double] Double
eval (Val d) = pure d
eval (Op prim) = case prim of
  Add x y -> evalBinary Add (+) x y
  Sub x y -> evalBinary Sub (-) x y
  Mul x y -> evalBinary Mul (*) x y
  Div x y -> evalBinary Div (/) x y
  Abs x   -> evalUnary  Abs abs x
  Sgn x   -> evalUnary  Sgn signum x

evalBinary :: (Double -> Double -> Prim Double)
           -> (Double -> Double -> Double)
           -> Expr -> Expr
           -> State [Prim Double] Double
evalBinary con op x y = do
  a <- eval x
  b <- eval y
  modifyState (con a b :)
  pure (op a b)

evalUnary :: (Double -> Prim Double)
          -> (Double -> Double)
          -> Expr
          -> State [Prim Double] Double
evalUnary con op x = do
  a <- eval x
  modifyState (con a :)
  pure (op a)
