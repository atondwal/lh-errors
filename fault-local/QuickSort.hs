module ListSort (quickSort) where


{-@ type OList a = [a]<{\fld v -> (v >= fld)}> @-}

------------------------------------------------------------------------------
-- Quick Sort ----------------------------------------------------------------
------------------------------------------------------------------------------

{-@ quickSort :: (Ord a) => [a] -> OList a @-}
quickSort []     = []
quickSort (x:xs) = append x lts gts 
  where 
    lts          = quickSort [y | y <- xs, y > x] -- supposed to be [y | y<-xs, y<x]
    gts          = quickSort [z | z <- xs, z >= x]

append k []     ys  = k : ys
append k (x:xs) ys  = x : append k xs ys


