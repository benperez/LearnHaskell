import Data.List (genericLength, partition, sort)

type Die = Int

score :: Die -> Int
score n = case n of 
  1 -> 1
  2 -> 2
  3 -> 0
  4 -> 4
  5 -> 5
  6 -> 6

score2 :: (Die, Die) -> (Int, Int)
score2 (a, b) = (score a, score b)

score3 :: (Die, Die, Die) -> (Int, Int, Int)
score3 (a, b, c) = (score a, score b, score c)

oneRoll :: [Die]
oneRoll = map score [1..6]

twoRolls :: [(Die, Die)]
twoRolls = map score2 [(a, b) | a <- [1..6], b <- [1..6]]

threeRolls :: [(Die, Die, Die)]
threeRolls = map score3 [(a, b, c) | a <- [1..6], b <- [1..6], c <- [1..6]]

average :: (Real a, Fractional b) => [a] -> b
average xs = realToFrac (sum xs) / genericLength xs

probability :: Fractional b => (a -> Bool) -> [a] -> b
probability f xs = (genericLength (filter f xs)) / (genericLength xs)

--evOneRoll = 3
evOneRoll :: Float
evOneRoll = average oneRoll

--evTwoRolls = 4.3888893
evTwoRolls :: Float
evTwoRolls = let shouldStay = \(a, b) -> a <=3 && b <=3
                 (stay, go) = partition shouldStay twoRolls
                 pPullBoth  = probability shouldStay twoRolls
                 evPullBoth = average $ map (uncurry (+)) $ stay
                 pRollAgain = 1 - pPullBoth
                 evLowerDie = average $ map (uncurry (min)) $ go
             in pPullBoth * evPullBoth + pRollAgain * (evLowerDie + evOneRoll)

--evThreeRolls = 5.233807
evThreeRolls :: Float
evThreeRolls = let
                 sortRoll (a, b, c) = map fromIntegral $ sort [a, b, c]
                 three  = \[a, b, c] -> a + b + c
                 two    = \[a, b, _] -> a + b + evOneRoll
                 one    = \[a, _, _] -> a + evTwoRolls
                 bestEv = foldl1 min . ([one, two, three] <*>) . pure . sortRoll
               in
                 average $ map bestEv threeRolls

--  Strategy for 1 roll 
--    None, take the die you roll
--  Strategy for 2 rolls 
--    Take the higher die if it is 1, 2, 3. Reroll when it is 4, 5, 6
--  Strategy for 3 rolls
--    Given a roll a, b, c where a <= b <= c, you must satisfy the following:
--    b + 3 <= 4.3888893 (or b <= 1.3888893) and b + c <= 4.3888893. This means
--    that you should always take a 0 or 1 if it is your middle die, and pull
--    both die if they add up to 4 or less.
--  Strategy for 4 rolls
--    Given a roll a, b, c, d where a <= b <= c <= d, you must satisfy the following:
--    b + 4.3888893 <= 5.233807 (or b <= 0.8449178). This means that you should only ever
--    pull 0's when rolling 4 die.
--  Strategy for 5 rolls
--    The difference in EV between 3 and 4 rolls will be smaller than the difference in EV
--    between 2 and 3, so the same strategy as 4 rolls applies. Only pull 0's.
