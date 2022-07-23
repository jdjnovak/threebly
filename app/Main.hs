module Main where

import Text.Read

data Error = ParseError String
  | TypeError String

data Command = Def String
  | Set String String
  | Out String
  | In Type

data Type = Str String | Num Int | None

type Variable = (String, String, Type)

-------------------------
-- HELPERS FOR STRINGS --
-------------------------

-- See if char is a quote, only supports double quotes
isQuote :: Char -> Bool
isQuote '"' = True -- ONLY SUPPORTING DOUBLE QUOTES
isQuote _ = False

-- Counts numbers of quotes in string
countQuote :: String -> Int
countQuote str = countQuoteHelper 0 str
  where
    countQuoteHelper :: Int -> String -> Int
    countQuoteHelper num "" = num
    countQuoteHelper num (x:xs) = if isQuote x then (countQuoteHelper (num+1) xs) else countQuoteHelper num xs

-- Returns first character, or Nothing if empty
getFirstChar :: String -> Maybe Char
getFirstChar "" = Nothing
getFirstChar (x:_) = Just x

-- Returns last character, or Nothing if empty
getLastChar :: String -> Maybe Char
getLastChar str = recToLast str
  where
    recToLast :: String -> Maybe Char
    recToLast (x:xs) = case length (x:xs) of
      0 -> Nothing
      1 -> Just x
      _ -> recToLast xs

-- Returns True if an input string is properly a string:
-- i.e., double quotes around text: "Hello, World!", not 'Hello, World!'
isString :: String -> Bool
isString "" = False
isString str = case getFirstChar str of
  Nothing -> False
  Just c -> (isQuote c) && case getLastChar str of
    Nothing -> False
    Just c' -> isQuote c' && (countQuote str == 2)


-------------------------
-- HELPERS FOR PARSING --
-------------------------

-- https://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell
-- Splits a string on a given condition
splitStringWhen :: (Char -> Bool) -> String -> [String]
splitStringWhen cond s = case dropWhile cond s of
                     "" -> []
                     s' -> w : splitStringWhen cond s''
                               where (w, s'') = break cond s'

-- Only do ints & strings for now
parseSet :: String -> String -> Either Error Variable
parseSet name value = case isString value of
  False -> case (readMaybe value :: Maybe Int) of
    Just v -> Right ("int", name, Num v)
    Nothing -> Left $ TypeError "TypeError: Only support Integers and Strings denoted by double quotes."
  True -> Right ("str", name, Str value)



main :: IO ()
main =
  do 
    let lines = splitStringWhen (=='\n') "set myvar 10\nset myvar2 12"
    print lines
