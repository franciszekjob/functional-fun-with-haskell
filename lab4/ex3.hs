
-- record syntax
-- data Cart3DVec' Int Int Int= Cart3DVec' {xCoord :: Int, yCoord :: Int, zCoord :: Int} deriving (Show)


-- funkcja obliczjaaca pole
data Shape = Circle Float | Rectange Float Float

area :: Shape -> Float
area (Circle radius) = pi * radius * radius
area (Rectange a b) = a * b

type TrafficLights = String

actionFor :: TrafficLights -> String
actionFor "Red" = "Stoj"
actionFor "Green" = "Jedz"
actionFor "Orange" = "Jedz szybko"


data BinIntTree = EmptyIntBT |
                  IntNodeBT Int BinIntTree BinIntTree

sumBinIntTree :: BinIntTree -> Int
sumBinIntTree EmptyIntBT = 0
sumBinIntTree (IntNodeBT n lt rt) = n + sumBinIntTree lt + sumBinIntTree rt

data BinTree a = EmptyBT |
                 NodeBT a (BinTree a) (BinTree a)

sumBinTree :: (Num a) => BinTree a -> a
sumBinTree EmptyBT = 0
sumBinTree (NodeBT n lt rt) = n + sumBinTree lt + sumBinTree rt


data Expr a = Lit a | -- literal/value a, e.g. Lit 2 = 2
              Add (Expr a) (Expr a)

eval :: Num a => Expr a -> a
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2

show' :: Show a => Expr a -> String
show' (Lit n) = show n
show' (Add e1 e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"


data BinTree a = EmptyBT | NodeBT a (BinTree a) (BinTree a) deriving (Show)

-- Function to calculate the depth of a binary tree
depthOfBT :: BinTree a -> Int
depthOfBT EmptyBT = 0
depthOfBT (NodeBT _ left right) = 1 + max (depthOfBT left) (depthOfBT right)

-- Function to flatten a binary tree using different traversal orders
flattenBT :: BinTree a -> [a]
flattenBT EmptyBT = []
flattenBT (NodeBT value left right) =
  -- Preorder traversal
  value : flattenBT left ++ flattenBT right

-- Function to map a function over a binary tree
mapBT :: (a -> b) -> BinTree a -> BinTree b
mapBT _ EmptyBT = EmptyBT
mapBT f (NodeBT value left right) = NodeBT (f value) (mapBT f left) (mapBT f right)

-- Function to insert an element into a Binary Search Tree (BST)
insert :: Ord a => a -> BinTree a -> BinTree a
insert x EmptyBT = NodeBT x EmptyBT EmptyBT
insert x (NodeBT value left right)
  | x < value = NodeBT value (insert x left) right
  | x > value = NodeBT value left (insert x right)
  | otherwise = NodeBT value left right  -- Ignore duplicates

-- Function to convert a list to a Binary Search Tree (BST)
list2BST :: Ord a => [a] -> BinTree a
list2BST = foldr insert EmptyBT
