{-# LANGUAGE BangPatterns #-}

import Data.List (unfoldr)

-- Naive recursive (exponential) fibonacci

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Tail recursive (linear) fibonacci

fib2 :: Integer -> Integer
fib2 n = go n (0, 1)
  where
    go !n (!a, !b)
      | n == 0    = a
      | otherwise = go (n - 1) (b, a + b)

fibs2 :: [Integer]
fibs2 = map fromIntegral $ unfoldr (\(!a,!b) -> Just (a,(b,a+b))) (0,1)

-- Stream base functionality

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ys = Cons x (interleaveStreams ys xs)

-- Stream implementations

nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

--0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, ...
--0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, ...
ruler :: Stream Integer
ruler = go 0
  where
    go n = interleaveStreams (streamRepeat n) (go (n + 1))
