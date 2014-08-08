module Golf where

-- skips "ABCD"  => ["ABCD","BD","C","D"]
-- skips "hello" => ["hello!","el!","l!","l","o","!"]
-- skips [True, False] => [[True, False], [False]]
skips :: [a] -> [[a]]
skips l = map (\n -> everyn n l) [1..(length l)]
    where everyn n = map head .
                         takeWhile (not . null) .
                         iterate (drop n) .
                         drop (n - 1)

--localMaxima [2,9,5,6,1] == [9,6]
--localMaxima [2,3,4,1,5] == [4]
--localMaxima [1,2,3,4,5] == []
localMaxima :: [Integer] -> [Integer]
localMaxima l = map fst $ filter snd $ zip l both
    where left = [(+ 1) . head $ l] ++ init l
          right = tail l ++ [(+ 1) . last $ l]
          gtleft = zipWith (>) l left
          gtRight = zipWith (>) l right
          both = zipWith (&&) gtleft gtRight
