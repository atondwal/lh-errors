module Zero where

-- We should automatically find /-by-0
-- and try to find the minimum source

zero :: a -> Integer
zero _ = 0

f :: Integer -> a -> Integer
f n d = n `div` zd + n `div` zd
        where zd = zero d
