module ListSort (insertSort, insertSort') where


{-@ type OList a = [a]<{\fld v -> (v >= fld)}> @-}

------------------------------------------------------------------------------
-- Insert Sort ---------------------------------------------------------------
------------------------------------------------------------------------------

{-@ insertSort :: (Ord a) => xs:[a] -> OList a @-}
insertSort            :: (Ord a) => [a] -> [a]
insertSort []         = []
insertSort (x:xs)     = insert x (insertSort xs) 

{-@ insertSort' :: (Ord a) => xs:[a] -> OList a @-}
insertSort'           :: (Ord a) => [a] -> [a]
insertSort' xs        = foldr insert [] xs

insert y []                   = [y]
insert y (x : xs) | y <= x    = x : y : xs  -- this is supposed to be y : x : xs
                  | otherwise = x : insert y xs

