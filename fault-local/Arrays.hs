module Arrays () where

f l i = l !! i

g m n = m - n

main = print $ f [1,2,3] $ g 3 5

-- problem: error is at the call site, not at the definition
