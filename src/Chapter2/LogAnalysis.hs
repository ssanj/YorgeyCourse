{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log
import Data.List.Split
import Data.List (isInfixOf)
import Data.Char

-- TODO: Refactor
-- exercise 1
parseMessage :: String -> LogMessage
parseMessage l = createMessages $ splitOn " " l
                  where
                    createMessages :: [String] -> LogMessage
                    createMessages tokens@("I":_) | length tokens >= 3 =
                      let ts = (head . drop 1) tokens in
                      if isNumeric ts then
                        LogMessage Info (getTimestamp ts) (unwords $ drop 2 tokens)
                      else Unknown $ unwords tokens  
                    
                    createMessages tokens@("W":_) | length tokens >= 3 =
                      let ts = (head . drop 1) tokens in
                      if isNumeric ts then  
                        LogMessage Warning (getTimestamp ts) (unwords $ drop 2 tokens)
                      else Unknown $ unwords tokens                      

                    createMessages tokens@("E":_) | length tokens >= 4 =
                      let sv = (head . drop 1) tokens
                          ts =  (head . drop 2) tokens
                      in 
                      if isSeverity sv && isNumeric ts then
                        LogMessage (Error $ getSeverity sv) (getTimestamp ts) (unwords $ drop 3 tokens)
                      else Unknown $ unwords tokens  

                    createMessages line =  Unknown $ unwords line

parse :: String -> [LogMessage]
parse content = map parseMessage (lines content)

isNumeric :: String -> Bool
isNumeric [] = False
isNumeric (x:[]) = isNumber x
isNumeric (x:xs) = isNumber x && isNumeric xs

isSeverity :: String -> Bool
isSeverity xs = isNumeric xs && (let val = (read xs :: Int) in val >= 1 && val <= 100)

getTimestamp :: String -> Int
getTimestamp s = read s :: Int

getSeverity :: String -> Int
getSeverity s = read s :: Int

-- exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) ms = ms
insert lm Leaf = Node Leaf lm Leaf
insert (LogMessage {}) un@(Node _ (Unknown _) _) = un
insert lmi@(LogMessage _ tsi _) (Node left lme@(LogMessage _ tse _) right) = 
  if tsi > tse then
    Node left lme (insert lmi right)
  else Node (insert lmi left) lme right

-- exercise 3
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node _ (Unknown _) _) = []
inOrder (Node left lm@(LogMessage {}) right) = inOrder left ++  [lm] ++ inOrder right

-- exercise 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs = map getMessage (relevant 50 $ sortByTimestamp xs)

containingWord :: String -> [LogMessage] -> [String]
containingWord w xs = filter (isInfixOf w) $ map getMessage $ sortByTimestamp xs

getMessage :: LogMessage -> String
getMessage (LogMessage _ _ msg) = msg
getMessage (Unknown _) = ""

sortByTimestamp :: [LogMessage] -> [LogMessage]
sortByTimestamp = inOrder . build

relevant :: Int -> [LogMessage] -> [LogMessage]
relevant severity = filter matchingSeverity 
                    where
                         matchingSeverity :: LogMessage -> Bool
                         matchingSeverity (LogMessage (Error sev) _ _) | sev > severity = True
                         matchingSeverity _ = False

-- verification methods

verify1 :: LogMessage
verify1 = parseMessage "E 2 562 help help"

verify2 :: LogMessage
verify2 = parseMessage "I 29 la la la"

verify3 :: LogMessage
verify3 = parseMessage "This is not in the right format"

verify4 :: IO [LogMessage]
verify4 = testParse parse 10 "error.log"

verify5 :: IO [String]
verify5 = testWhatWentWrong parse whatWentWrong "sample.log"

verify6 :: IO [String]
verify6 = testWhatWentWrong parse whatWentWrong "error.log"

verify7 :: IO [String]
verify7 = testWhatWentWrong parse (containingWord "mustard") "error.log"

