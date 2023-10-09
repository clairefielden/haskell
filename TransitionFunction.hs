module TransitionFunction where 

--import Prelude hiding (Left, Right)
import Data.List (intercalate, intersperse, transpose)

tablefy :: [String] -> [[String]] -> String
tablefy h rs
    | any (/= length h) (map length rs) = error "Tablefy.tablefy: Differences in length"
    | otherwise                         = table
    where
        table  = unlines $ insert' sep (header:rows)
        widths = map (maximum . map length) (transpose (h:rs))
        sep    = insert "+" $ map (flip replicate '-' . (+2)) widths
        header = mkRow Center h
        rows   = map (mkRow L) rs
        mkRow a       = insert "|" . zipWith (mkCell a) widths
        mkCell a n xs = " " ++ pad a n ' ' xs ++ " "

insert :: [a] -> [[a]] -> [a]
insert x xs = intercalate x ([] : xs ++ [[]])

insert' :: [a] -> [[a]] -> [[a]]
insert' x xs = intersperse x ([] : xs ++ [[]])

data Alignment = L | R | Center deriving Eq

pad :: Alignment -> Int -> a -> [a] -> [a]
pad a n x xs
    | n < 1          = error "Tablefy.pad: Length must not be smaller than one"
    | n <= length xs = take n xs
    | a == L         = xs ++ replicate (n - length xs) x
    | a == R         = replicate (n - length xs) x ++ xs
    | a == Center    = let (space, extra) = quotRem (n - length xs) 2
                       in replicate space x ++ xs ++ replicate (space + extra) x

data T = T { a :: String
           , b :: Char
           , c :: Char
           , d :: String
           , e :: String}
    deriving Show

transitionfunction = [
                     ["Start","_","_","Right","Start"], 
                     ["Start","1",".","Right","ScanRight"], 
                     ["Start","0","0","Right","Subtract"],
                     ["ScanRight","1","1","Right","ScanRight"],
                     ["ScanRight", "0", "0", "Right", "ArgA"],
                     ["ArgA" , "." , "." , "Right" , "ArgA"],
                     ["ArgA" , "_" , "_" , "Right" , "Accept"],
                     ["ArgA" , "1" , "." , "Left" , "ScanLeft"],
                     ["ScanLeft" , "." , "." , "Left" , "ScanLeft"],
                     ["ScanLeft" , "0" , "0" , "Left" , "ArgB"],
                     ["ArgB" , "1" , "1" , "Right" , "ArgB"],
                     ["ArgB" , "." , "." , "Right" , "Start"],
                     ["Subtract" , "." , "_" , "Right" , "Subtract"],
                     ["Subtract" , "1" , "1" , "Right" , "Halt"]]
