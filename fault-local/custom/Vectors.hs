{-@ LIQUID "--short-names" @-}
{-@ LIQUID "--no-termination" @-}

import Prelude hiding (reverse, map)

infixr 9 :+:

-- | A simple `Vector` data type from scratch:
data Vector a = Emp
              | (:+:) a (Vector a)
                deriving (Eq, Ord, Show)

{-@ measure size @-}
size :: Vector a -> Int
size Emp        = 0
size (x :+: xs) = 1 + size xs

{-@ invariant {v:Vector a | 0 <= size v} @-}

-- | Type alias for Vector of a given size N

{-@ type VectorN a N  = {v:Vector a | size v = N} @-}

---------------------------------------------------------------
-- | Append ---------------------------------------------------
---------------------------------------------------------------

-- refinement should be size xs + size ys
{-@ append :: xs:Vector a -> ys:Vector a -> VectorN a {size xs - size ys} @-}
append Emp ys        = ys
append (x :+: xs) ys = x :+: append xs ys
