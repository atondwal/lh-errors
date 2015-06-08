{-@ LIQUID "--no-termination" @-}

{-@ data Node v = Nil | Node (c :: v) (r :: Node {a:v|a<=c} ) (l :: Node {a:v|a>c} ) @-}

data Node v = Nil | Node v (Node v) (Node v)

{-@ measure rightNode :: Node a -> Node a@-}
rightNode (Node _ r _) = r
rightNode Nil = Nil

{-@ measure leftNode :: Node a -> Node a@-}
leftNode (Node _ _ l) = l
leftNode Nil = Nil

singleton v = Node v Nil Nil

-- | Buggy Function
bstInsert :: (Ord v) => v -> Node v -> Node v
bstInsert v Nil = singleton v
-- The simplest way to fix is
-- Node c r l
bstInsert v (Node c l r) = if v > c
                             then Node c r (bstInsert v l)
                             else Node c (bstInsert v r) l
