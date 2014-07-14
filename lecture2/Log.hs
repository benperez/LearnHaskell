-- CIS 194 Homework 2

module Log where

import Control.Applicative

data MessageType = Info
                 | Warning
                 | Error Int
  deriving (Show, Eq)

type TimeStamp = Int

data LogMessage = LogMessage MessageType TimeStamp String
                | Unknown String
  deriving (Show, Eq)

data MessageTree = Leaf
                 | Node MessageTree LogMessage MessageTree
  deriving (Show, Eq)

parseMessage :: String -> LogMessage
parseMessage s = case words s of
            ("I":t:msg)   -> LogMessage Info (read t) (unwords msg)
            ("W":t:msg)   -> LogMessage Warning (read t) (unwords msg)
            ("E":l:t:msg) -> LogMessage (Error $ read l) (read t) (unwords msg)
            _             -> Unknown s

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t                  = t
insert m           Leaf               = Node Leaf m Leaf
insert msg_new@(LogMessage _ t1 _)  (Node lt msg@(LogMessage _ t2 _) rt)
  | t1 < t2   = Node (insert msg_new lt) msg rt
  | otherwise = Node lt msg (insert msg_new rt)

build :: [LogMessage] -> MessageTree
build = foldr (insert) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf         = []
inOrder (Node l m r) = inOrder l ++ [m] ++ inOrder r

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map toString
              . inOrder
              . build
              . filter messageRelevant

messageRelevant :: LogMessage -> Bool
messageRelevant (LogMessage (Error lvl) _ _)
  | lvl >= 50 = True
  | otherwise = False
messageRelevant _ = False

toString :: LogMessage -> String
toString (LogMessage _ _ s) = s

-- | @testParse p n f@ tests the log file parser @p@ by running it
--   on the first @n@ lines of file @f@.
testParse :: (String -> [LogMessage])
          -> Int
          -> FilePath
          -> IO [LogMessage]
testParse parse n file = take n . parse <$> readFile file

-- | @testWhatWentWrong p w f@ tests the log file parser @p@ and
--   warning message extractor @w@ by running them on the log file
--   @f@.
testWhatWentWrong :: (String -> [LogMessage])
                  -> ([LogMessage] -> [String])
                  -> FilePath
                  -> IO [String]
testWhatWentWrong parse whatWentWrong file
  = whatWentWrong . parse <$> readFile file
