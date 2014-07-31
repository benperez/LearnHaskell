module Golf where
import Control.Category ((>>>))

-- skips "ABCD"  => ["ABCD","BD","C","D"]
-- skips "hello" => ["hello!","el!","l!","l","o","!"]
-- skips [True, False] => [[True, False], [False]]
skips :: [a] -> [[a]]
skips l = map (\n -> everyn n l) [1..(length l)]

everyn :: Int -> [a] -> [a]
everyn n = map head .
               takeWhile (not . null) .
               iterate (drop n) .
               drop (n - 1)

-- alternatively
everyn' :: Int -> [a] -> [a]
everyn' n = drop (n - 1) >>>
            iterate (drop n) >>>
            takeWhile (not . null) >>>
            map head

--localMaxima :: [Integer] -> [Integer]
--localMaxima l = map fst ( filter (\xs -> ((fst xs) == (foldl1 max xs))) (zip3 l l l) )