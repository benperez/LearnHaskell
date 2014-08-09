module Golf where
import Data.List (group, intersperse, sort, transpose)

-- skips "ABCD"  => ["ABCD","BD","C","D"]
-- skips "hello" => ["hello!","el!","l!","l","o","!"]
-- skips [True, False] => [[True, False], [False]]
skips :: [a] -> [[a]]
skips l = map (\n -> everyn n l) [1..(length l)]
  where everyn n = map head .
                   takeWhile (not . null) .
                   iterate (drop n) .
                   drop (n - 1)

-- localMaxima [2,9,5,6,1] == [9,6]
-- localMaxima [2,3,4,1,5] == [4]
-- localMaxima [1,2,3,4,5] == []
localMaxima :: [Integer] -> [Integer]
localMaxima l = map fst $ filter snd $ zip l gtlr
  where left  = [succ . head $ l] ++ init l
        right = tail l ++ [succ . last $ l]
        gtlr  = zipWith (&&)
                        (zipWith (>) l left)
                        (zipWith (>) l right)

-- histogram [1,1,1,5] ==
--  *
--  *
--  *   *
-- ==========
-- 0123456789
-- histogram [1,4,5,4,6,6,3,4,2,4,9] ==
--     *
--     *
--     * *
--  ******  *
-- ==========
-- 0123456789
histogram :: [Integer] -> String
histogram l = unlines .
              reverse .
              (++) ["==========\n0123456789"] .
              transpose $
              zipWith (++) stars empties
  where starCounts  = map (pred . length) . group . sort . (++ [0..9]) $ l
        emptyCounts = map ((-) (maximum starCounts)) starCounts
        stars       = map ((flip replicate) '*') starCounts
        empties     = map ((flip replicate) ' ') emptyCounts
