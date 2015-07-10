module Zero (f) where

zero d = 0 

f n d = let zd = zero d in n `div` (zero d) + n `div` (zero d)

