import Data.List (group, reverse, sort, transpose)

allUnique :: String -> Bool
allUnique = any ((>1) . length) . group . sort

areAnagrams :: String -> String -> Bool
areAnagrams s1 s2 = (sort s1) == (sort s2)

escapeSpaces :: String -> String
escapeSpaces [] = []
escapeSpaces (x:xs)
  | x == ' '  = '%':'2':'0':(escapeSpaces xs)
  | otherwise = x:(escapeSpaces xs)

numberToSound :: Int -> String
numberToSound n | div3 && div5 = "fizzbuzz"
                | div3         = "fizz"
                | div5         = "buzz"
                | otherwise    = show n
  where
    div3 = n `mod` 3 == 0
    div5 = n `mod` 5 == 0

main :: IO ()
main = mapM_ (putStrLn . numberToSound) [1..15]
