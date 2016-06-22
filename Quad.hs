class Quadable a where
    quad:: a -> a -> Int
    in1:: a -> a -> Bool
    in2:: a -> a -> Bool
    in3:: a -> a -> Bool
    in4:: a -> a -> Bool
    quad x y | in1 x y = 0
             | in2 x y = 1
             | in3 x y = 2
             | in4 x y = 3
    in1 x y = quad x y == 0
    in2 x y = quad x y == 1
    in3 x y = quad x y == 2
    in4 x y = quad x y == 3

data PointInt2D = Point Int Int deriving (Show)
data QuadTree a = Node (a) [QuadTree a] | Empty deriving (Show)

instance Quadable (PointInt2D) where
    in1 (Point a1 b1) (Point a2 b2) = ((&&) (a1 <= a2) (b1 <= b2))
    in2 (Point a1 b1) (Point a2 b2) = ((&&) (a1 > a2)  (b1 <= b2))
    in3 (Point a1 b1) (Point a2 b2) = ((&&) (a1 > a2)  (b1 > b2))
    in4 (Point a1 b1) (Point a2 b2) = ((&&) (a1 <= a2) (b1 > b2))


q = Node (Point 0 1) [Node (Point 1 3) [Empty, Empty, Empty, Node (Point 2 2) [Empty, Empty, Empty, Empty]],
                          Node (Point (-3) 2) [Empty, Empty, Empty, Empty],
                          Node (Point (-3) (-2)) [Node (Point (-1) 0) [Empty, Empty, Empty, Empty], Empty, Empty, Empty],
                          Node (Point 5 0) [Empty, Empty, Empty, Empty]]

isEmpty :: QuadTree a -> Bool
isEmpty Empty = True
isEmpty _ = False

insert :: Quadable a => a -> QuadTree a -> QuadTree a
insert a1 (Node (a2) l) =
    if isEmpty x then
        Node (a2) ((take n l) ++ [(Node (a1) [Empty,Empty,Empty,Empty])] ++ (drop (n+1) l))
    else
        Node (a2) ((take n l) ++ [(insert a1 x)] ++ (drop (n+1) l))
        where
        n = quad a2 a1
        x = (l!!n)


