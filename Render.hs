module Render where

import Types
import Parser
import SVG

-- draw current game state
raceToSvg :: FilePath -> Track -> [Trace] -> IO ()
raceToSvg path track traces = do
                                linesToSvg path (getViewBox track) ((trackToSvgList track) ++ createTraces traces ++ movementToSvgList (stateFromTrace (head traces)))
                                return ()

-- calculate min and max xs and ys for viewbox from track
getViewBox :: Track -> (Int, Int, Int, Int)
getViewBox track =  (floor (minimum x), floor (minimum y), ceiling (maximum x), ceiling (maximum y))
                    where
                        x = (getxs [(second track)]) ++ (getxs (third track))
                        y = (getys [(second track)]) ++ (getys (third track))

-- gets a list of all x-coordinates of the track
getxs :: [Line] -> [Float]
getxs [] = []
getxs (x:xs) =  [a, b] ++ getxs xs
                where
                    ((a, _),(b, _)) = x

-- gets a list of all y-coordinate of the track
getys :: [Line] -> [Float]
getys [] = []
getys (y:ys) =  [a, b] ++ getys ys
                where
                    ((_, a),(_, b)) = y

-- converts track to a list of SVGs
trackToSvgList :: Track -> [Svg]
trackToSvgList track = (finishLineToSvgListBetter (second track))++(linesToSvgList "black" "0.1" (third track))

-- fancy checkeboard finish line
finishLineToSvgListBetter :: Line -> [Svg]
finishLineToSvgListBetter fline = (finishLineToSvgList fline1 0) ++ (finishLineToSvgList fline 0.1) ++ (finishLineToSvgList fline2 0)
                                where
                                    fline1 = ((xa, ya - 0.4),(xb, yb - 0.4))
                                    fline2 = ((xa, ya + 0.4),(xb, yb + 0.4))
                                    ((xa, ya), (xb, yb)) = fline

-- striped lines
finishLineToSvgList :: Line -> Float -> [Svg]
finishLineToSvgList fline 1.2 = []
finishLineToSvgList fline 1.1 = []
finishLineToSvgList fline t = (renderLine "red" "0.4" ((fx1 + dx*t, fy1 + dy*t), (fx1 + dx*(t+0.1), fy1 + dy*(t+0.1)))) : finishLineToSvgList fline (t + 0.2)
                            where
                                ((fx1, fy1), (fx2, fy2)) = fline
                                (dx, dy) = (fx2 - fx1, fy2 - fy1)

linesToSvgList :: String -> String -> [Line] -> [Svg]
linesToSvgList x y = map (renderLine x y)

-- creates movement crosshair
movementToSvgList :: CarState -> [Svg]
movementToSvgList state =   linesToSvgList "green" "0.05" [a, b, c, d]
                            where
                                ((ox, oy), (vx, vy)) = state
                                (xx, yy) = positionToPoint (ox + vx, oy + vy)
                                a = ((xx - 1, yy - 1), (xx + 1, yy + 1))
                                b = ((xx - 1, yy + 1), (xx + 1, yy - 1))
                                c = ((xx, yy - 1), (xx, yy + 1))
                                d = ((xx - 1, yy), (xx + 1, yy))

-- converts trace to list of lines
traceToLines :: Trace -> [Line]
traceToLines [] = []
traceToLines [x] = []
traceToLines (x:xs) = (positionToPoint x, positionToPoint (head xs)):(traceToLines xs)

-- renders current trace in blue and other player's trace in gray
createTraces :: [Trace] -> [Svg]
createTraces [x]   = linesToSvgList "blue" "0.1" (traceToLines x)
createTraces (x:y) = linesToSvgList "gray" "0.1" (traceToLines (head y)) ++ linesToSvgList "blue" "0.1" (traceToLines x)

-- used to determine carstate from trace -- only necessary because we were not allowed to add CarState as a parameter to raceToSvg
stateFromTrace :: Trace -> CarState
stateFromTrace [x] = (x, (0, 0))
stateFromTrace (x:xs) =   (x, (ax - bx, ay - by))
                            where
                                ((ax, ay), (bx, by)) = (x, head  xs)