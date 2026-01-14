module HW4.T1
  ( EvaluationError(..)
  , ExceptState(..)
  , mapExceptState
  , wrapExceptState
  , joinExceptState
  , modifyExceptState
  , throwExceptState
  , eval
  ) where

import HW4.Types

data ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f (ES run) = ES $ \s ->
  case run s of
    Error e           -> Error e
    Success (a :# s') -> Success (f a :# s')

wrapExceptState :: a -> ExceptState e s a
wrapExceptState a = ES $ \s -> Success (a :# s)

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState (ES outer) = ES $ \s ->
  case outer s of
    Error e                  -> Error e
    Success (ES inner :# s') -> inner s'

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES $ \s -> Success (() :# f s)

throwExceptState :: e -> ExceptState e s a
throwExceptState e = ES $ \_ -> Error e

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  esf <*> esa = ES $ \s ->
    case runES esf s of
      Error e        -> Error e
      Success (f :# s') ->
        case runES esa s' of
          Error e           -> Error e
          Success (a :# s'') -> Success (f a :# s'')

instance Monad (ExceptState e s) where
  esa >>= f = joinExceptState (mapExceptState f esa)

data EvaluationError = DivideByZero
  deriving Show

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Val d) = pure d
eval (Op prim) = case prim of
  Add x y -> evalBinary Add (+) x y
  Sub x y -> evalBinary Sub (-) x y
  Mul x y -> evalBinary Mul (*) x y
  Div x y -> evalDiv x y
  Abs x   -> evalUnary Abs abs x
  Sgn x   -> evalUnary Sgn signum x

evalBinary :: (Double -> Double -> Prim Double)
           -> (Double -> Double -> Double)
           -> Expr -> Expr
           -> ExceptState EvaluationError [Prim Double] Double
evalBinary con op x y = do
  a <- eval x
  b <- eval y
  modifyExceptState (con a b :)
  pure (op a b)

evalDiv :: Expr -> Expr
        -> ExceptState EvaluationError [Prim Double] Double
evalDiv x y = do
  a <- eval x
  b <- eval y
  if b == 0
    then throwExceptState DivideByZero
    else do
      modifyExceptState (Div a b :)
      pure (a / b)

evalUnary :: (Double -> Prim Double)
          -> (Double -> Double)
          -> Expr
          -> ExceptState EvaluationError [Prim Double] Double
evalUnary con op x = do
  a <- eval x
  modifyExceptState (con a :)
  pure (op a)
