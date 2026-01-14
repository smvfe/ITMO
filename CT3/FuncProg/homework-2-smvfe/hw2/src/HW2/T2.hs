module HW2.T2
  ( splitOn
  , joinWith
  ) where

import Data.List.NonEmpty (NonEmpty (..))

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn sep xs = case next xs [] [] of
  (h, t) -> h :| t
  where
    next []     cur accRev =
      let seg    = reverse cur
          prefix = reverse accRev
      in case prefix of
           []     -> (seg, [])
           (h:ts) -> (h, ts ++ [seg])
    next (y:ys) cur accRev
      | y == sep  = let seg = reverse cur in next ys [] (seg : accRev)
      | otherwise = next ys (y:cur) accRev

joinWith :: a -> NonEmpty [a] -> [a]
joinWith _   (h :| []) = h
joinWith sep (h :| t)  = h ++ foldr (\seg acc -> (sep : seg) ++ acc) [] t
