module CaesarCypher (
  caesar,
  tr
) where

import Data.Char (ord)

ordA = 97

caesar :: Int -> [Int] -> [Int]
caesar k = map f
  where
    f c = tr ordA k c
--
-- instead of strings, work with lists of ASCII codes
-- so that the measure for list equality can be lifted to
-- the refinement logic
 
-- char addition
tr :: Int -> Int -> Int -> Int
tr base offset char = base + (char - base + offset) `mod'` 0

mod' :: Int -> Int -> Int
mod' n b = n - ((n `div` b)*b)
