{- Example of AVL trees by michaelbeaumont -}

{-@ LIQUID "--totality" @-}
module AVL (Tree, singleton, insert) where

-- Basic functions
data Tree a = Nil | Tree a (Tree a) (Tree a) deriving Show

ht              :: Tree a -> Int
ht Nil          = 0
ht (Tree x l r) = if (ht l) > (ht r) then (1 + ht l) else (1 + ht r)


bFac Nil          = 0
bFac (Tree v l r) = ht l - ht r

htDiff :: Tree a -> Tree a -> Int
htDiff l r = ht l - ht r

emp = Nil

singleton a = Tree a Nil Nil

-- | Insert functions

{-@ Decrease insert 3 @-}
insert :: (Ord a) => a -> Tree a -> Tree a
insert a Nil = singleton a
insert a t@(Tree v l r) = case compare a v of
    LT -> insL
    GT -> insR
    EQ -> t
    where r' = insert a r
          l' = insert a l
          insL | siblDiff > 1
                 && bFac l' == (0-1) = rebalanceLR v l' r
               | siblDiff > 1
                 && bFac l' == 1  = rebalanceLL v l' r
               | siblDiff <= 1 = Tree v l' r
               | otherwise = t
               where siblDiff = htDiff l' r
          insR | siblDiff > 1
                 && bFac r' == 1 = rebalanceRL v l r'
               | siblDiff > 1
                 && bFac r' == (0-1) = rebalanceRR v l r'
               | siblDiff <= 1 = Tree v l r'
               | otherwise = t
               where siblDiff = htDiff r' l

rebalanceLL v (Tree lv ll lr) r                 = Tree lv ll (Tree v lr r)

rebalanceLR v (Tree lv ll (Tree lrv lrl lrr)) r = Tree lrv (Tree lv ll lrl) (Tree v lrr r)

rebalanceRR v l (Tree rv rl rr)                 = Tree rv (Tree v l rl) rr

rebalanceRL v l (Tree rv (Tree rlv rll rlr) rr) = Tree rlv (Tree v l rll) (Tree rv rlr rr) 

-- Test
main = do
    mapM_ print [a,b,c,d]
  where
    {-@ a :: Tree Integer @-}
    a = singleton 5
    {-@ b :: Tree Integer @-}
    b = insert 2 a
    {-@ c :: Tree Integer @-}
    c = insert 3 b
    {-@ d :: Tree Integer @-}
    d = insert 7 c
