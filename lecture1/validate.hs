import Data.Char

-- Convert a number to its component digits
-- toDigits 1234 => [1,2,3,4]
-- toDigits 0 = []
toDigits :: Integer -> [Integer]
toDigits n 
  | n > 0  = map (toInteger . digitToInt) (show n)
  | otherwise [] 

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

-- Doubles every other integer, from the right
-- doubleEveryOther [8,7,6,5] == [16,7,12,5]
-- doubleEveryOther [1,2,3] == [1,4,3]
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse
                 . zipWith (*) (cycle [1,2])
                 . reverse

-- Calculate the sum of all digits of the given numbers
-- sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22
sumDigits :: [Integer] -> Integer
sumDigits = sum 
          . concatMap toDigits 

-- Whether or not an Integer would be a valid credit card no.
-- validate 4012888888881881 = True
validate 4012888888881882 = False
validate :: Integer -> Bool
validate = (==) 0 
         . flip mod 10
         . sumDigits
         . doubleEveryOther
         . toDigits
