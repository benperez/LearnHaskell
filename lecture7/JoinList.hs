{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module JoinList where

import Data.Monoid
import Data.Maybe
import Data.Map (Map, fromList, lookup)

import Buffer
import Editor
import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
left +++ right = Append (mappend (tag left) (tag right)) left right

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

sz :: (Monoid b, Sized b) => JoinList b a -> Int
sz = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int ->
                                 JoinList b a ->
                                 Maybe a
indexJ n _ | n < 0      = Nothing
indexJ 0 Empty          = Nothing
indexJ 0 (Single _ v)   = Just v
indexJ n (Append m l r)
  | n < sz l  = indexJ n l
  | otherwise = indexJ (n - sz l) r
indexJ _ _              = Nothing

testlist = ((Single (Size 1) 'y') +++ ((Single (Size 1) 'e') +++ (Single (Size 1) 'a')) +++ (Single (Size 1) 'h'))

dropJ :: (Sized b, Monoid b) => Int ->
                                JoinList b a ->
                                JoinList b a
dropJ n jl | n < 1     = jl
dropJ _ Empty          = Empty
dropJ _ (Single _ _)   = Empty
dropJ n (Append m l r)
  | n < sz l  = dropJ n l +++ r
  | otherwise = dropJ (n - (sz l)) r

takeJ :: (Sized b, Monoid b) => Int ->
                                JoinList b a ->
                                JoinList b a
takeJ n jl | n < 1     = Empty
takeJ _ Empty          = Empty
takeJ n s@(Single _ _)
  | n >= 1    = s
  | otherwise = Empty
takeJ n (Append m l r) = takeJ n l +++ (takeJ (n - (sz l)) r)

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

instance Monoid Score where
  mempty  = Score 0
  mappend = (+)

scoreMap :: Data.Map.Map Char Score
scoreMap = fromList [ ('a', Score 1)
                    , ('b', Score 3)
                    , ('c', Score 3)
                    , ('d', Score 2)
                    , ('e', Score 1)
                    , ('f', Score 4)
                    , ('g', Score 2)
                    , ('h', Score 4)
                    , ('i', Score 1)
                    , ('j', Score 8)
                    , ('k', Score 5)
                    , ('l', Score 1)
                    , ('m', Score 3)
                    , ('n', Score 1)
                    , ('o', Score 1)
                    , ('p', Score 3)
                    , ('q', Score 10)
                    , ('r', Score 1)
                    , ('s', Score 1)
                    , ('t', Score 1)
                    , ('u', Score 1)
                    , ('v', Score 4)
                    , ('w', Score 4)
                    , ('x', Score 8)
                    , ('y', Score 4)
                    , ('z', Score 10) ]
score :: Char -> Score
score c = case Data.Map.lookup c scoreMap of
  Just v  -> v
  Nothing -> mempty

scoreString :: String -> Score
scoreString = mconcat . map score

scoreLine :: String -> JoinList Score String
scoreLine line = Single (scoreString line) line

instance Buffer (JoinList (Score, Size) String) where
  toString = show

  fromString = foldr (+++) Empty .
               map (\line -> Single (scoreString line, Size 1) line) .
               lines

  line = indexJ

  replaceLine n line jl = takeJ (n - 1) jl +++
                          (Single (scoreString line, Size 1) line) +++
                          (dropJ n jl)

  numLines = length . jlToList

  value Empty                       = 0
  value (Single ((Score n), _) _)   = n
  value (Append ((Score n), _) _ _) = n

main = runEditor editor jlbuffer
  where jlbuffer = fromString $ unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ] :: (JoinList (Score, Size) String)
