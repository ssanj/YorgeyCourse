{-# OPTIONS_GHC -Wall #-}

module LogAnalysis2 where

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
                    createMessages xs | isInfoMessage xs =
                      let ts = toTS (xs !! 1)
                          msg = unwords $ drop 2 xs
                      in LogMessage Info ts msg

                    createMessages xs | isWarningMessage xs =  
                      let ts = toTS (xs !! 1)
                          msg = unwords $ drop 2 xs
                      in LogMessage Warning ts msg

                    createMessages xs | isErrorMessage xs =  
                      let sev = toSeverity (xs !! 1)
                          ts = toTS (xs !! 2)
                          msg = unwords $ drop 3 xs
                      in LogMessage (Error sev) ts msg                      
                                      
                    createMessages line =  Unknown $ unwords line

parse :: String -> [LogMessage]
parse content = map parseMessage (lines content)

isInfoMessage :: [String] -> Bool
isInfoMessage xs = iwTokens xs && isInfoType (head xs) && isTS (xs !! 1)

isWarningMessage :: [String] -> Bool
isWarningMessage xs = iwTokens xs && isWarningType (head xs) && isTS (xs !! 1)

isErrorMessage :: [String] -> Bool
isErrorMessage xs = errorTokens xs && isErrorType (head xs) && isSeverity (xs !! 1) && isTS (xs !! 2)
 
isInfoType :: String -> Bool
isInfoType = ("I" ==)

isWarningType :: String -> Bool
isWarningType = ("W" ==)

isErrorType :: String -> Bool
isErrorType = ("E" ==)

-- has at least 3 tokens needed by Info and Warning message types.
iwTokens :: [String] -> Bool
iwTokens = atleastTokens 3

errorTokens :: [String] -> Bool
errorTokens = atleastTokens 4

atleastTokens :: Int -> [String] -> Bool
atleastTokens n xs = length xs >= n

isNumeric :: String -> Bool
isNumeric = all isDigit

isTS :: String -> Bool
isTS = isNumeric

isSeverity :: String -> Bool
isSeverity xs = isNumeric xs && (let val = (read xs :: Int) in val >= 1 && val <= 100)

toTS :: String -> Int
toTS s = read s :: Int

toSeverity :: String -> Int
toSeverity s = read s :: Int

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

