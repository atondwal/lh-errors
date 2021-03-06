module Evens where

{-@ type Even = {v:Int | v mod 2 = 0} @-}

{-@ isEven :: n:Nat -> {v:Bool | ((Prop v) <=> (n mod 2 = 0))} @-}   
isEven   :: Int -> Bool 
isEven 0 = True
isEven 1 = False
isEven n = isEven (n-1)

{-@ evens :: n:Nat -> [Even] @-}
evens n = [i | i <- range 0 n, isEven i] 

{-@ range :: lo:Int -> hi:Int -> [{v:Int | (lo <= v && v < hi)}] / [hi -lo] @-}
range :: Int -> Int -> [Int]
range lo hi 
  | lo < hi   = lo :  range (lo+1) hi
  | otherwise = []

