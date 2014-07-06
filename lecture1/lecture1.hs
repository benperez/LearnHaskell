import Data.Char

toDigits :: Integer -> [Integer]
toDigits n
  | n > 0     = map (toInteger . digitToInt) (show n)
  | otherwise = []

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

doubleHelper :: [Integer] -> [Integer]
doubleHelper []       = []
doubleHelper [x]      = [x]
doubleHelper (x:y:xs) = [x,y*2] ++ doubleHelper xs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther l = reverse ( doubleHelper ( reverse l ) )

sumDigits :: [Integer] -> Integer
sumDigits l = sum (map (sum . toDigits) l)

validate :: Integer -> Bool
validate n = sumDigits ( doubleEveryOther (toDigits n) ) `mod` 10 == 0