module CaesarCypher (
  caesar,
) where

import Data.Char (ord)

{-@ caesar :: Int -> x:[Int] -> { y:[Int] | difference (elems x) (elems y)) = empty } @-}
caesar :: Int -> [Int] -> [Int]
caesar k = map f
  where
    f c = tr (ord 'a') k c
--
-- instead of strings, work with lists of ASCII codes
-- so that the measure for list equality can be lifted to
-- the refinement logic
 
-- char addition
{-@ tr :: x:Int -> Int -> Int -> {v:Int | v >= x && v < x+26} @-}
tr :: Int -> Int -> Int -> Int
tr base offset char = base + (char - base + offset) `mod'` 26

{-@ mod' :: x:Int -> y:{Int | y > 0} -> {v:Int | v >= 0 && v < y} @-}
mod' :: Int -> Int -> Int
mod' n b = n - ((n `div` b)*b)
