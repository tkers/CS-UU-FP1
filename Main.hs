-- RaceTrack by Tijn Kersjes (3855473) and Jordi Vermeulen (3835634)
-- For one or two players
-- Improved visuals: more appealing (variable) stroke width, PRO CHECKERBOARD FINISH, movement crosshair

module Main where

import Types
import Parser
import Render
import Mechanics

main :: IO ()
main = do
        track <- readTrack "muh.rtr"
        let start = ([(first track)], ((first track), (0, 0)))
        print "Number of players? (1/2)"
        inp <- getLine
        if (readInt inp == 2)
            then multiPlayerGameLoop track start start
            else singlePlayerGameLoop track start

multiPlayerGameLoop :: Track -> (Trace, CarState) -> (Trace, CarState) ->  IO ()
multiPlayerGameLoop track (a, b) (c, d) = do
                                            (a, b, e1) <- gameLoop track a c b
                                            (c, d, e2) <- gameLoop track c a d
                                            if (e1 || e2)
                                                then return ()
                                                else multiPlayerGameLoop track (a, b) (c, d)
                                                
singlePlayerGameLoop :: Track -> (Trace, CarState) ->  IO ()
singlePlayerGameLoop track (a, b) = do
                                        (a, b, e1) <- gameLoop track a [] b
                                        if (e1)
                                            then return ()
                                            else singlePlayerGameLoop track (a, b)

                                                
gameLoop :: Track -> Trace -> Trace -> CarState -> IO (Trace, CarState, Bool)
gameLoop track trace otherTrace state = do
                                            raceToSvg "race.svg" track [trace, otherTrace]
                                            a <- getLine
                                            -- determine new car position and velocity
                                            let inp = readInt a
                                                ((x, y), (v, w)) = state
                                                (v2, w2) = (v + processHorizontal inp, w + processVertical inp)
                                                (x2, y2) = (x + v2, y + w2)
                                            -- check for finish and crash
                                            if (completes state (x2, y2) track)
                                                then do print ("Finished in " ++ show (length trace) ++ " moves!")
                                                        raceToSvg "race.svg" track [(x2, y2):trace] -- draw last move before finish
                                                        return (trace, state, True)
                                                else if (not (crashes state (x2, y2) track))
                                                        then return (((x2, y2):trace), ((x2, y2), (v2, w2)), False)
                                                        else do print "Crash!"
                                                                raceToSvg "race.svg" track ([(x2, y2):trace]) -- draw last move after crash
                                                                return (trace, state, True)
                                                               
-- calculate horizontal acceleration based on input
processHorizontal :: Int -> Int
processHorizontal a =   if (a `mod` 3 == 0)
                            then 1
                            else if (a `mod` 3 == 1)
                                    then -1
                                    else 0

-- calculate vertical acceleration based on input
processVertical :: Int -> Int
processVertical a =   if (a <= 3)
                        then 1
                        else if (a >= 7)
                                then -1
                                else 0