import Control.Monad.State
import Data.List (intersperse, nub, find)
import System.Exit

import TransitionFunction

data TapeMovement = MoveLeft | MoveRight | Stay deriving (Show, Eq)
-- Rule = (state 1, input, output, movement, state 2)
type Rule a = (a, a, a, TapeMovement, a)
-- Execution = (tape position, current machine state, tape)
type Execution a = (Int, a, [a])
type Log a = [Execution a]
type UTM a b = State (Machine a) b

-- can work with data of any type
data Machine a = Machine
    { allStates :: [a] -- not used actually
    , initialState :: a  -- not used actually, initial state in "current"
    , finalStates :: [a]
    , symbols :: [a]     -- not used actually
    , blank :: a
    , noOpSymbol :: a    -- means: don't change input / don't shift tape
    , rules :: [Rule a]
    , current :: Execution a
    , machineLog :: Log a -- stores state changes from last to first
    , machineLogActive :: Bool -- if true, intermediate steps are stored
    , noRuleMsg :: a -- error symbol if no rule matches
    , stopMsg :: a } -- symbol to append to the end result
    deriving (Show)

-- it is not checked whether the input and output symbols are valid
apply :: Eq a => Rule a -> UTM a a
apply (_, _, output, direction, stateUpdate) = do
    m <- get
    let (pos, currentState, tape) = current m
        tapeUpdate = if output == noOpSymbol m
            then tape
            else take pos tape ++ [output] ++ drop (pos + 1) tape
        newTape
            | pos == 0 && direction == MoveLeft = blank m : tapeUpdate
            | succ pos == length tape && direction == MoveRight = tapeUpdate ++ [blank m]
            | otherwise = tapeUpdate
        newPosition = case direction of
            MoveLeft -> if pos == 0 then 0 else pred pos
            MoveRight -> succ pos
            Stay -> pos
        newState = if stateUpdate == noOpSymbol m
            then currentState
            else stateUpdate
    put $! m { current = (newPosition, newState, newTape) }
    return newState

-- rules with no-operation symbols and states must be underneath
-- rules with defined symbols and states
lookupRule :: Eq a => UTM a (Maybe (Rule a))
lookupRule = do
    m <- get
    let (pos, currentState, tape) = current m
        item = tape !! pos
        isValid (e, i, _, _, _) = e == currentState &&
            (i == item || i == noOpSymbol m)
    return $! find isValid (rules m)

msgToLog :: a -> UTM a ()
msgToLog e = do
    m <- get
    let (pos, currentState, tape) = current m
    put $! m { machineLog = (pos, currentState, tape ++ [e]) : machineLog m }

toLog :: UTM a ()
toLog = do
    m <- get
    put $! m { machineLog = current m : machineLog m }

-- execute the machine's program
execute :: Eq a => UTM a ()
execute = do
    toLog -- log the initial state
    loop
    where
        loop = do
            m <- get
            r <- lookupRule -- look for a matching rule
            case r of
                Nothing -> msgToLog (noRuleMsg m)
                Just rule -> do
                    stateUpdate <- apply rule
                    if stateUpdate `elem` finalStates m
                        then msgToLog (stopMsg m)
                        else do
                            when (machineLogActive m) toLog
                            loop

---------------------------
-- convenient functions
---------------------------

-- run execute, format and print the output
runMachine :: Machine String -> IO ()
runMachine m@(Machine { current = (_, _, tape) }) =
    if null tape
        then putStrLn "NO TAPE"
        else case machineLog $ execState execute m of
                [] -> putStrLn "NO OUTPUT"
                xs -> do
                    mapM_ (\(pos, _, output) -> do
                        let formatOutput = concat output
                        putStrLn formatOutput
                        putStrLn (replicate pos ' ' ++ "^")) $ reverse xs
                    putStrLn $ show (length xs) ++ " STEPS. FINAL STATE: " ++
                        let (_, finalState, _) = head xs in show finalState

-- convert a string with format state+space+input+space+output+space+
-- direction+space+new state to a rule

toRule :: String -> Rule String
toRule xs =
    let [a, b, c, d, e] = take 5 $ words xs
        dir = case d of
            "l" -> MoveLeft
            "r" -> MoveRight
            "*" -> Stay
    in  (a, b, c, dir, e)

-- load a text file and parse it to a machine.
-- see comments and examples
-- lines in the file starting with ';' are header lines or comments
-- header and input lines must contain a ':' and after that the content to be parsed
-- so there can be comments between ';' and ':' in those lines

loadMachine :: FilePath -> IO (Machine String)
loadMachine n = do
    f <- readFile n

    let ls = lines f
        -- header: first 4 lines
        ([e1, e2, e3, e4], rest) = splitAt 4 ls
        -- rules and input: rest of the file
        re = map toRule . filter (not . null) $ map (takeWhile (/= ';')) rest
        ei = head . words . tail . snd $ break (== ':') e1
        va = head . words . tail . snd $ break (== ':') e3
        ci = words . intersperse ' ' . tail . snd $ break (== ':') $ last rest

    return Machine
        { rules = re
        , initialState = ei
        , finalStates = words . tail . snd $ break (== ':') e2
        , blank = va
        , noOpSymbol = head . words . tail . snd $ break (== ':') e4
        , allStates = nub $ concatMap (\(a, _, _, _, e) -> [a, e]) re
        , symbols = nub $ concatMap (\(_, b, c, _, _) -> [b, c]) re
        , current = (0, ei, if null ci then [va] else ci)
        -- we assume
        , noRuleMsg = "\tNO RULE." -- error: no matching rule found
        , stopMsg = "\tHALT." -- message: machine reached a final state
        , machineLog = []
        , machineLogActive = True }

calculateUnary :: String -> String -> String
calculateUnary a b = do
    let suma = length $ filter (=='1') a
    let sumb = length $ filter (=='1') b
    if any (/='1') (a ++ b) then "Argument must consist of only 1's"
    else if (suma - sumb) < 1 then "Argument 1 should be greater than Argument 2"
    else "; Initial tape: _" ++ a ++ "0" ++ b ++ "_" 

checkLength :: String -> String -> String
checkLength a b = do
    if length a == 0 || length a > 32 then "Argument 1 is of incorrect length!"
    else if length b == 0 || length b > 32 then "Argument 2 is of incorrect length!"
    else calculateUnary a b

takeword = takeWhile (' ' /=)

fallOverAndDie :: String -> IO a
fallOverAndDie err = do putStrLn err
                        putStrLn "This Language is not Turing Recognizable" 
                        exitWith (ExitFailure 1)

recognizable :: String -> String -> Int
recognizable a b = (length a - length b)

checkArgs :: String -> String -> IO()
checkArgs x y = do
       let result = checkLength x y
       if takeword result == "Argument" then fallOverAndDie result
       else putStrLn (result)

getTape :: String -> String -> String
getTape x y = "; Initial tape: _" ++ x ++ "0" ++ y ++ "_" 

main :: IO()
main = do
        putStrLn "A Universal Turing Machine has the following language: "
        putStrLn "UTM = {< M , arg >}"
        --putStrLn "M = A Turing Machine to simulate, we will use the following: "
        --putStrLn (tablefy ["Current State", "Input Symbol", "Output Symbol", "Move", "Next State"] transitionfunction)
        putStrLn "arg = Binary form of the Turing Machine's tape"
        putStrLn "The Turing Machine takes two arguments in the form of unary numbers"
        putStrLn "Argument 1:"
        x <- getLine
        putStrLn "Argument 2:"
        y <- getLine
        checkArgs x y
        let result = getTape x y
        putStrLn "The following tape is finite, beginning and ending with a blank"
        putStrLn "0 indicates the end of Argument 1 and start of Argument 2"
        inMachine <- readFile "M.txt"
        outMachine <- writeFile "turing.txt" inMachine
        appendFile "turing.txt" result
        loadMachine "turing.txt" >>= runMachine
        putStrLn "This Language is Turing Recognizable" 
        putStr "Result: "
        print (length x - length y)