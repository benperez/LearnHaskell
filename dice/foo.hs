import Data.List (group, sort)

allUnique :: String -> Bool
allUnique = any ((>1) . length) . group . sort

areAnagrams :: String -> String -> Bool
areAnagrams s1 s2 = (sort s1) == (sort s2)

escapeSpaces :: String -> String
escapeSpaces [] = []
escapeSpaces (x:xs)
  | x == ' '  = '%':'2':'0':(escapeSpaces xs)
  | otherwise = x:(escapeSpaces xs)

--rotate an M by N matrix 90 degrees
newtype Matrix = Matrix { bytes :: [[Int]]} 

rotateMatrix :: Matrix -> Matrix 
rotateMatrix = 


--regular jawn
[[1,2],
 [3,4]]

--transposed jawn
[[1,3],
 [2,4]]

--rotated jawn
[[3,1],
 [4,2]]
