{-# LANGUAGE BangPatterns #-}

import Data.List (unfoldr)

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fib2 :: Integer -> Integer
fib2 n = go n (0, 1)
  where
    go !n (!a, !b)
      | n == 0    = a
      | otherwise = go (n - 1) (b, a + b)

fibs2 :: [Integer]
fibs2 = map fromIntegral $ unfoldr (\(!a,!b) -> Just (a,(b,a+b))) (0,1)
