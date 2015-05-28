module Blank where

import Prelude hiding ((++))
import Language.Haskell.Liquid.Prelude

data Tree a = Leaf a 
            | Node (Tree a) (Tree a)

{-@
data Tree [tlen] a = Leaf (l::a) 
                   | Node (l::(Tree a)) (r::(Tree a))
  @-}

{-@ tSum        :: t:(Tree Int) -> {v:Int | v = (tSum t)} @-}
tSum            :: Tree Int -> Int
tSum (Leaf x)   = x
tSum (Node l r) = tSum l + tSum r

{-@ toList         :: t:(Tree Int) -> {v: [Int] | (lSum v) = (tSum t)} @-}
toList             :: Tree Int -> [Int] 
toList (Leaf x)    = [x]
toList (Node l r)  = (toList l) ++ (toList r)

{-@ Blank.++           :: xs:[Int] -> ys:[Int] -> {v: [Int] | (lSum v) = (lSum xs) + (lSum ys)} @-}
(++)               :: [Int] -> [Int] -> [Int]
[]     ++ ys       = ys
(x:xs) ++ ys       = x : (xs ++ ys)

-----------------------------------------------------
-- Redefine Measures For Parsing Reasons ------------
-----------------------------------------------------

{-@ 
measure lSum     :: [Int] -> Int
lSum ([])        = 0
lSum (x:xs)      = x + (lSum xs)
@-}

{-@ 
measure tSum     :: Tree Int -> Int
tSum (Leaf x)    = x
tSum (Node l r)  = 1 + (tSum l) + (tSum r)
@-}
-- Node pattern should not + 1

{-@ 
measure tlen     :: (Tree a) -> Int
tlen (Leaf x)    = 0
tlen (Node l r)  = 1 + (tlen l) + (tlen r)
@-}

{-@ invariant {v:Tree a | (tlen v) >= 0} @-}

