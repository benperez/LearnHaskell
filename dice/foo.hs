import Data.List (group, reverse, sort, transpose)
import Data.Monoid

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
numberToSound n
  | not (div3 || div5) = show n
  | otherwise          = mappend (if div3 then "fizz" else mempty)
                                 (if div5 then "buzz" else mempty)
  where
    div3 = n `mod` 3 == 0
    div5 = n `mod` 5 == 0

main :: IO ()
main = mapM_ (putStrLn . numberToSound) [1..15]

myreverse :: [a] -> [a]
myreverse l = rev l []
  where rev [] a       = a
        rev (x:xs) acc = rev xs (x:acc)
