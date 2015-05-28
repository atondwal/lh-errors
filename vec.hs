---------------------------------------------------------------
-- User-defined Lists
---------------------------------------------------------------

-- The earlier definition is baked into LiquidHaskell's prelude,
-- since its for GHC Lists.
-- For completeness, lets illustrate the method on a new list type.

data Vec a = Null | Cons a (Vec a)

{-@ data Vec a <p :: a -> a -> Prop> 
      = Null
      | Cons (h :: a) (t :: Vec <p> (a<p h>))
  @-}

{-@ type IncrVec a = Vec <{\xi xj -> xi <= xj}> a @-}

{-@ digsVec :: IncrVec Int @-}
digsVec     = Cons 0 (Cons 1 (Cons 2 Null))

