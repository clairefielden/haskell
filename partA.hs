module PartA where

import Data.Array
import Control.Applicative
import Data.Char
import System.Environment
import System.IO
import Data.List (intercalate)

--QUESTION 1
 
halve :: [a] -> ([a],[a])
halve xs
    | null xs           = error "Can't halve an empty list!"
    | even (length xs)  = (take half xs, drop half xs)
    | otherwise         = error "Can't halve an odd-length list!"
    where half = length xs `div` 2

--QUESTION 2

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

containsCharacters :: [a] -> Bool
containsCharacters [] = False  -- Empty list does not contain characters
containsCharacters (x:xs) = case x of
  _ -> True  -- If the first element is not an empty list, it contains characters
  --_ -> containsCharacters xs  -- Recursively check the rest of the list

newList :: Show a => [a] -> String
newList c = "[" ++ intercalate "," (map show (reverse c)) ++ "]"
 
reverseList :: Show a => [a] -> IO()
reverseList input
     | containsCharacters input == True     =  print (newList input)
     | otherwise                            =  print (reverse' input)

--QUESTION 3

reverseSecondhalf :: [a] -> [a]
reverseSecondhalf xs
    | length xs == 8           = concat [fst (halve xs) , reverse (snd (halve xs))]
    | otherwise                = error "List must have a length of 8"

--QUESTION 4

--guarded equation implementation
safetaila :: [a] -> [a]
safetaila list
     | null list        = []
     | otherwise        = drop 1 list

--pattern matching implementation
safetailb :: [b] -> [b]
safetailb [] = []
safetailb x = tail x