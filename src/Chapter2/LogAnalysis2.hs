{-# OPTIONS_GHC -Wall #-}

module Chapter2.LogAnalysis2 (parseMessage,
                     parse,
                     insert,
                     build,
                     inOrder,
                     whatWentWrong,
                     safeParseInt,
                     verify1,
                     verify2,
                     verify3,
                     verify4,
                     verify5,
                     verify6,
                     verify7)
 where

import Chapter2.Log
import Data.List (isInfixOf)
import Data.Char
import Data.Maybe (listToMaybe, fromMaybe)
import Control.Applicative
import System.FilePath.Posix

-- LogFormat
-- ^[I|W|(E Severity)] Timestamp Text$

-- exercise 1
parseMessage :: String -> LogMessage
parseMessage line = case words line of
                      "I" : ts : msg | isTS ts -> LogMessage Info (toTS ts) (unwords msg)
                      "W" : ts : msg | isTS ts -> LogMessage Warning (toTS ts) (unwords msg)
                      "E" : sv : ts : msg | isSeverity sv && isTS ts -> LogMessage (Error (toSeverity sv)) (toTS ts) (unwords msg)
                      _ -> Unknown line

parse :: String -> [LogMessage]
parse content = map parseMessage (lines content)


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
whatWentWrong = map getMessage . filter (matchingSeverity 50) . sortByTimestamp

containingWord :: String -> [LogMessage] -> [String]
containingWord w = filter (isInfixOf w) . map getMessage . sortByTimestamp

getMessage :: LogMessage -> String
getMessage (LogMessage _ _ msg) = msg
getMessage (Unknown _) = ""

sortByTimestamp :: [LogMessage] -> [LogMessage]
sortByTimestamp = inOrder . build

matchingSeverity :: Int -> LogMessage -> Bool
matchingSeverity severity (LogMessage (Error sev) _ _) | sev > severity = True
matchingSeverity _ _ = False

-- more safety functions
safeParseInt :: String -> Maybe Int
safeParseInt = listToMaybe . map fst . filter (null . snd) . reads

safeParseMessage :: String -> LogMessage
safeParseMessage s = fromMaybe (Unknown s) $ case words s of
    "I" : l : xs -> buildM (pure Info) l xs
    "W" : l : xs -> buildM (pure Warning) l xs
    "E" : e : l : xs -> buildM (Error <$> safeParseInt e) l xs
    _ -> Nothing
  where
    buildM mt l xs = LogMessage <$> mt <*> safeParseInt l <*> pure (unwords xs)

-- less duplication
leanerParseMessage :: String -> LogMessage
leanerParseMessage msg = case words msg of
    "I" : xs -> mkMessage Info xs
    "W" : xs -> mkMessage Warning xs
    "E" : err : xs -> mkMessage (mkError err) xs
    _ -> Unknown msg
 where
    mkError s             = Error (read s)
    mkMessage _ []        = Unknown msg
    mkMessage mt (t : xs) = LogMessage mt (read t) (unwords xs)


-- verification methods

basePath :: FilePath
basePath = "src/Chapter2"

errorLog :: FilePath
errorLog = basePath </> "error.log"

sampleLog :: FilePath
sampleLog = basePath </> "sample.log"

verify1 :: LogMessage
verify1 = parseMessage "E 2 562 help help"

verify2 :: LogMessage
verify2 = parseMessage "I 29 la la la"

verify3 :: LogMessage
verify3 = parseMessage "This is not in the right format"

verify4 :: IO [LogMessage]
verify4 = testParse parse 10 (errorLog)

verify5 :: IO [String]
verify5 = testWhatWentWrong parse whatWentWrong sampleLog

verify6 :: IO [String]
verify6 = testWhatWentWrong parse whatWentWrong errorLog

verify7 :: IO [String]
verify7 = testWhatWentWrong parse (containingWord "mustard") errorLog

