data Balance = R | L | C

{-@ data Node v <p :: Bool -> v -> v -> Prop> = Leaf v | Node Balance (r :: Node <p> v<p True c>) (c :: v) (l :: Node <p> v<p False c>) @-}

data Node v = Leaf v | Node Balance (Node v) v (Node v)

{-@ measure height @-}
height :: Node a -> Integer
height (Leaf _) = 1
height (Node _ r _ l) = 1 + if ρ > λ then ρ else λ
        where ρ = height r
              λ = height l

{-@ measure balanced @-}
balanced :: Node a -> Bool
balanced (Leaf _) = True
balanced (Node b r _ l) = balanced r && balanced l && height r == c + height l
        where c = case b of R -> 1
                            C -> 0
                            L -> -1


{-@ type BST a = Node <{\b v n -> (n<=v) <=> b==True}> a @-}

{-@ testTree :: BST Integer @-}
testTree :: Node Integer
testTree = Node C (Leaf 1) 2 (Leaf 3)

{-@ testTree2 :: BST Integer @-}
testTree2 :: Node Integer
testTree2 = Node C (Leaf 5) 2 (Leaf 3)
