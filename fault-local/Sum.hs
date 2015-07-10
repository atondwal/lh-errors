module Sum () where

{-@ sum2 :: k:Int -> {v:Int | v >= 0 && v >= k } @-}
sum2 :: Int -> Int
sum2 k = if k <= 0 then 0 else let s = sum2 (k-1) in s + k

