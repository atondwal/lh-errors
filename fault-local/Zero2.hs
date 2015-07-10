
{-@ zero :: Int -> { v:Int | v /= 0 } @-}
zero :: Int -> Int
zero d = 0

f n d = let zd = zero d in n `div` zd + n `div` zd
