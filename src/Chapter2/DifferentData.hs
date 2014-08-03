module DifferentData where

import Data.List.Split
import Data.Char

type Timestamp = Int

data MessageType = Info
                 | Warning
                 | Error Int
  deriving (Show, Eq)

data LogMessage = LogMessage Timestamp String deriving (Show, Eq)

data MessageTree = Leaf | Node MessageTree LogMessage MessageTree deriving (Show, Eq)

isNumeric :: String -> Bool
isNumeric [] = False
isNumeric (x:[]) = isNumber x
isNumeric (x:xs) = isNumber x && isNumeric xs

timestamp :: String -> Int
timestamp s = read s :: Int

parseMessage :: String -> Either String LogMessage
parseMessage line = let splits = splitOn " " line in
                    if length splits >= 3 && isNumeric (splits !! 1) then 
                    	Right (LogMessage (timestamp (splits !! 1)) (unwords $ drop 2 splits))
                    else Left (unwords splits)

parse :: String -> [Either String LogMessage]
parse = error

mType :: String -> MessageType
mType = error

hasWITokens :: String -> Bool
hasWITokens xs = length xs >= 3

hasETokens :: [String] -> Bool
hasETokens xs = length xs >= 4

isMessageType :: [String] -> Bool
isMessageType xs = (xs !! 0) `elem` ["I", "W", "E"]

hasWITimestamp :: [String] -> Bool
hasWITimestamp xs = all isDigit (xs !! 1)

hasETimestamp :: [String] -> Bool
hasETimestamp xs = all isDigit (xs !! 2)

hasSeverity :: [String] -> Bool
hasSeverity xs = all isDigit (xs !! 1)

isError :: [String] -> Bool
isError xs = hasETokens xs && hasSeverity xs && hasETimestamp xs
isError = hasETokens <*> hasSeverity <*> hasETimestamp

insert :: LogMessage -> MessageTree -> MessageTree
insert lg Leaf = Node Leaf lg Leaf
insert ilg@(LogMessage ints _) (Node left lg@(LogMessage ts _) right) = 
	if ints > ts then 
		Node left lg (insert ilg right)
	else Node (insert ilg left) lg right	

message :: LogMessage -> String
message (LogMessage _ msg) = msg