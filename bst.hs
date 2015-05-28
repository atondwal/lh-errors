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
putativeHeight Nil = 0
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

{-@ type AVL a = { v : Node a | balanced v && height v == putativeHeight v} @-}

{-@ testTreeEven :: AVL Integer @-}
testTreeEven :: Node Integer
testTreeEven = Node 2 2 (Node 1 1 Nil Nil) (Node 1 3 Nil Nil)

{-@ testTreeLeft :: AVL Integer @-}
testTreeLeft :: Node Integer
testTreeLeft = Node 3 3 (Node 2 1 (Node 1 0 Nil Nil) (Node 1 2 Nil Nil)) (Node 1 4 Nil Nil)

{-@ testTreeRight :: AVL Integer @-}
testTreeRight :: Node Integer
testTreeRight = Node 3 0 (Node 1 (-1) Nil Nil) (Node 2 2 (Node 1 1 Nil Nil) (Node 1 3 Nil Nil))

{-@ testTreeUnbal :: AVL Integer @-}
testTreeUnbal :: Node Integer
testTreeUnbal = Node 4 0 (Node 3 (-5) (Node 1 (-8) Nil Nil) (Node 2 (-2) (Node 1 (-3) Nil Nil) (Node 1 (-1) Nil Nil))) (Node 1 1 Nil Nil)

{-@ testTreeUnsearch :: AVL Integer @-}
testTreeUnsearch :: Node Integer
testTreeUnsearch = Node 2 0 (Node 1 1 Nil Nil) (Node 1 (-1) Nil Nil)

{-@ testTreeWrongH :: AVL Integer @-}
testTreeWrongH :: Node Integer
testTreeWrongH = Node 4 0 (Node 1 (-1) Nil Nil) (Node 2 2 (Node 1 1 Nil Nil) (Node 1 3 Nil Nil))

insert :: (Ord a) => a -> Node a -> (Maybe (), Balance, Node a)
insert x Nil = (Just (), C, Node 1 x Nil Nil)
insert x t@(Node h y l r)
     | x == y = (Nothing, bal, t)
     | x < y  = case insert x l of
                  (Nothing, λ) -> (Nothing,Node h λ y r)
                  (Just _ ,λ) ->
                    case bal of
                      C -> (Just _ , Node More λ y r)
                      L -> (Nothing, Node Same λ y r)
                      R -> rotR λ y r
     | x > y  = case insert x r of
                  (Nothing,ρ) -> (Nothing, Node h l y ρ)
                  (Just (),ρ) ->
                    case h of
                      C -> (Just (), Node Less l y ρ)
                      R -> (Nothing, Node Same l y ρ)
                      L -> rotL l y ρ
  where bal = 3
