{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log
import Data.List.Split
import Data.Char

-- TODO: Refactor
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