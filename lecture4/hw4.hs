fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product .
        map (subtract 2) .
        filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (> 1) . iterate collatz
  where collatz n = if even n then n `div` 2
                              else 3 * n + 1

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr packTree Leaf

packTree :: a -> Tree a -> Tree a
packTree v' Leaf = Node 0 Leaf v' Leaf
packTree v' (Node h left v right)
    | hl > hr   = Node h  left   v (packTree v' right)
    | hl < hr   = Node h  inLeft v right
    | otherwise = Node h' inLeft v right
  where inLeft  = packTree v' left
        heightTree Leaf = 0
        heightTree (Node n t1 val t2) = n
        hl = heightTree left
        hr = heightTree right
        h' = (heightTree inLeft) + 1

xor :: [Bool] -> Bool
xor =  not . even . length . filter id

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base $ reverse xs

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+1) . (*2)) . filter (not . (`elem` crossed_out)) $ [1..n]
  where crossed_out = filter (<= n) [i + j + 2 * i * j | i <- [1..n], j <- [i..n]]
