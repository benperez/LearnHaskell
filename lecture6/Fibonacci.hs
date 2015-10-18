{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

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

streamZip :: (a -> a -> a) -> Stream a -> Stream a -> Stream a
streamZip f (Cons x xs) (Cons y ys) = Cons (f x y) (streamZip f xs ys)

streamFilter :: (a -> Bool) -> Stream a -> Stream a
streamFilter f (Cons x xs) = case f x of
  True  -> Cons x (streamFilter f xs)
  False -> streamFilter f xs

-- Stream implementations

nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

--0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, ...
ruler :: Stream Integer
ruler = go 0
  where
    go n = interleaveStreams (streamRepeat n) (go (n + 1))

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger n = Cons n (streamRepeat 0)
  negate = streamMap negate
  (+) = streamZip (+)
  (Cons a0 aprime) * bs@(Cons b0 bprime) = Cons (a0 * b0)
                                                (streamZip (+)
                                                           (streamMap (*a0) bprime)
                                                           (aprime * bs))

instance Fractional (Stream Integer) where
  as@(Cons a0 aprime) / bs@(Cons b0 bprime) = Cons (quot a0 b0)
                                                   (streamMap (* (quot 1 b0))
                                                              (aprime - (as / bs) * bprime))

fibs3 = x / (1 - x - x^2)

-- Matrix fibonaccis
data Matrix = Matrix ((Integer, Integer), (Integer, Integer))
  deriving (Show)

toTuples :: Matrix -> ((Integer, Integer), (Integer, Integer))
toTuples (Matrix m) = m

instance Num Matrix where
  Matrix ((a, b), (c, d)) * Matrix ((e, f), (g, h)) =
    Matrix ((a*e + b*g, a*f + b*h), (c*e + d*g, c*f + d*h))

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = snd $ fst $ toTuples $ ((Matrix ((1, 1), (1, 0))) ^ n)
