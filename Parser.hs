module Parser where

import Types
import System.IO
import System.Environment
import Control.Monad

readFloat :: String -> Float
readFloat str = case reads str of
	[] -> error "not a floating point number"
	(p,_):_ -> p

readInt :: String -> Int
readInt str = case reads str of
	[] -> error "not an integer"
	(p,_):_ -> p

readTrack :: String -> IO Track
readTrack path = do
                    contents <- readFile path
                    let   x = (map words (lines contents))
                          start = readPosition (head x) -- start from first line
                          finish = readLine (head (tail x)) -- finish from second line
                          borders = map readLine (tail (tail x)) -- borders from rest of lines
                    return (start, finish, borders)

-- convert from list of strings to position
readPosition :: [String] -> Position
readPosition input = (readInt (head input), readInt (last input))

-- convert from list of strings to point
readPoint :: [String] -> Point
readPoint input = (readFloat (head input), readFloat (last input))

-- convert from list of strings to line
readLine :: [String] -> Line
readLine input = (readPoint (take 2 input), readPoint (drop 2 input))