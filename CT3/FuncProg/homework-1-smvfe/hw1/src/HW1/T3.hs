module HW1.T3
  ( Tree (..)
  , tsize
  , tdepth
  , tmember
  , tinsert
  , tFromList
  ) where

type Meta = (Int, Int) -- (size, depth)

data Tree a = Leaf | Branch Meta (Tree a) a (Tree a)
  deriving (Show)

------------------------------

mkbranch :: Tree a -> a -> Tree a -> Tree a
mkbranch l x r = Branch (1 + tsize l + tsize r, 1 + max (tdepth l) (tdepth r)) l x r

lrotate :: Tree a -> Tree a
lrotate (Branch _ a x (Branch _ b y c)) =
  mkbranch (mkbranch a x b) y c
lrotate t = t

rrotate :: Tree a -> Tree a
rrotate (Branch _ (Branch _ a x b) y c) =
  mkbranch a x (mkbranch b y c)
rrotate t = t

balance :: Tree a -> Tree a
balance Leaf = Leaf
balance t@(Branch _ l x r)
  | diff == 2 =
      case r of
        (Branch _ rl _ rr) | tdepth rl > tdepth rr -> lrotate (mkbranch l x (rrotate r))
        _                                          -> lrotate t
  | diff == -2 =
      case l of
        (Branch _ ll _ lr) | tdepth lr > tdepth ll -> rrotate (mkbranch (lrotate l) x r)
        _                                          -> rrotate t
  | otherwise = mkbranch l x r
  where
    diff = tdepth r - tdepth l

------------------------------

tsize :: Tree a -> Int
tsize Leaf                     = 0
tsize (Branch (size, _) _ _ _) = size

tdepth :: Tree a -> Int
tdepth Leaf                    = 0
tdepth (Branch (_, dep) _ _ _) = dep

-- Uncomment this function signature to implement this function.
-- The correct signature is commented to avoid compilation failure
-- with '-Werror' in the template project.
--
tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember x (Branch _ l y r)
  | x == y    = True
  | x < y     = tmember x l
  | otherwise = tmember x r

-- Uncomment this function signature to implement this function.
-- The correct signature is commented to avoid compilation failure
-- with '-Werror' in the template project.
tinsert :: Ord a => a -> Tree a -> Tree a
tinsert y Leaf = mkbranch Leaf y Leaf
tinsert y t@(Branch _ l x r)
  | y < x     = balance (mkbranch (tinsert y l) x r)
  | y > x     = balance (mkbranch l x (tinsert y r))
  | otherwise = t

-- Uncomment this function signature to implement this function.
-- The correct signature is commented to avoid compilation failure
-- with '-Werror' in the template project.
tFromList :: Ord a => [a] -> Tree a
tFromList = foldr tinsert Leaf
