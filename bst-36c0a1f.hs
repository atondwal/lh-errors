data Balance = R | C | L

{-@ data Node v = Nil | Node (h :: Balance) (c :: v) (r :: Node {a:v|a<c} ) (l :: Node {a:v|a>c} ) @-}

data Node v = Nil | Node Balance v (Node v) (Node v)

{-@ measure height @-}
height :: Node a -> Integer
height Nil = 0
height (Node _ _ r l) = if ρ > λ then ρ else λ
        where ρ = 1 + height r
              λ = 1 + height l

{-@ measure balanced @-}
balanced :: Node a -> Bool
balanced Nil = True
balanced (Node _ _ r l) = balanced r && balanced l && (height r) - (height l) <= 1 && (height r) - (height l) >= -1

{-@ measure putativeHeight @-}
putativeHeight Nil = C
putativeHeight (Node h _ _ _) = h

{-@ measure rightNode :: Node a -> Node a@-}
rightNode (Node _ _ r _) = r
rightNode Nil = Nil

{-@ measure leftNode :: Node a -> Node a@-}
leftNode (Node _ _ _ l) = l
leftNode Nil = Nil

{-@ measure toBal :: {v :Integer | v = 0 || v = 1 || v = (-1)} -> Balance @-}
toBal 0 = C
toBal (-1) = L
toBal 1 = R
toBal _ = error "Unbalanced AVL Tree"

{-@ type AVL a = { v : Node a | balanced v && toBal (height (rightNode v) - height (leftNode v)) == putativeHeight v} @-}

{-@ testTreeEven :: AVL Integer @-}
testTreeEven :: Node Integer
testTreeEven = Node C 2 (Node C 1 Nil Nil) (Node C 3 Nil Nil)

{-@ testTreeLeft :: AVL Integer @-}
testTreeLeft :: Node Integer
testTreeLeft = Node L 3 (Node C 1 (Node C 0 Nil Nil) (Node C 2 Nil Nil)) (Node C 4 Nil Nil)

{-@ testTreeRight :: AVL Integer @-}
testTreeRight :: Node Integer
testTreeRight = Node L 0 (Node C (-1) Nil Nil) (Node C 2 (Node C 1 Nil Nil) (Node C 3 Nil Nil))

{-@ testTreeUnbal :: AVL Integer @-}
testTreeUnbal :: Node Integer
testTreeUnbal = Node L 0 (Node R (-5) (Node C (-8) Nil Nil) (Node C (-2) (Node C (-3) Nil Nil) (Node C (-1) Nil Nil))) (Node C 1 Nil Nil)

{-@ testTreeUnsearch :: AVL Integer @-}
testTreeUnsearch :: Node Integer
testTreeUnsearch = Node C 0 (Node C 1 Nil Nil) (Node C (-1) Nil Nil)

{-@ testTreeWrongH :: AVL Integer @-}
testTreeWrongH :: Node Integer
testTreeWrongH = Node C 0 (Node C (-1) Nil Nil) (Node C 2 (Node C 1 Nil Nil) (Node C 3 Nil Nil))
