{-# LANGUAGE TypeOperators #-}

module HW0.T1
  ( type (<->)(Iso)
  , flipIso
  , runIso
  , distrib
  , assocPair
  , assocEither
  ) where

data a <-> b = Iso (a -> b) (b -> a)

assocPair :: (a, (b, c)) <-> ((a, b), c)
assocPair = Iso to from
  where
    to (a, (b, c))   = ((a, b), c)
    from ((a, b), c) = (a, (b, c))

assocEither :: Either a (Either b c) <-> Either (Either a b) c
assocEither = Iso to from
  where
    to (Left a)          = Left (Left a)
    to (Right (Left b))  = Left (Right b)
    to (Right (Right c)) = Right c
    from (Left (Left a))  = Left a
    from (Left (Right b)) = Right (Left b)
    from (Right c)        = Right (Right c)

flipIso :: (a <-> b) -> (b <-> a)
flipIso (Iso f g) = Iso g f

runIso :: (a <-> b) -> (a -> b)
runIso (Iso f _) = f

distrib :: Either a (b, c) -> (Either a b, Either a c)
distrib (Left a)       = (Left a, Left a)
distrib (Right (b, c)) = (Right b, Right c)

