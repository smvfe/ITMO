module HW2.T3
  ( mcat
  , epart
  ) where

import qualified Data.Foldable as F (foldMap)

mcat :: Monoid a => [Maybe a] -> a
mcat = F.foldMap (maybe mempty id)

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = F.foldMap $ \e -> case e of
  Left  a -> (a, mempty)
  Right b -> (mempty, b)
