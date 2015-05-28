import Data.Char (ord, chr)
import Data.Ix (inRange)
 
caesar :: Int -> String -> String
caesar k = map f
  where
    f c
      | inRange ('a','z') c = chr $ tr (ord 'a') k (ord c)
      | inRange ('A','Z') c = chr $ tr (ord 'A') k (ord c)
      | otherwise = c
 
unCaesar :: Int -> String -> String
unCaesar k = caesar (-k)
 
-- char addition
{-@ tr :: x:Int -> Int -> Int -> {v:Int | v >= x && v < x+26} @-}
tr :: Int -> Int -> Int -> Int
tr base offset char = base + (char - base + offset) `mod'` 26

{-@ mod' :: x:Int -> y:{Int | y > 0} -> {v:Int | v >= 0 && v < y} @-}
mod' :: Int -> Int -> Int
mod' n b = n - ((n `div` b)*b)
