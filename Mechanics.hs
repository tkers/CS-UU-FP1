module Mechanics where

import Render
import Types
import Parser

crashes :: CarState -> Position -> Track -> Bool
crashes state pos track = or (map (intersects (positionToPoint (fst state), positionToPoint pos)) (third track))

completes :: CarState -> Position -> Track -> Bool
completes state pos track = intersects (positionToPoint (fst state), positionToPoint pos) (second track)

moveValid :: CarState -> Position -> Bool
moveValid ((px, py),(vx, vy)) (posx, posy) = abs (speed (vx, vy) - speed (posx - px, posy - py)) <= sqrt 2

speed :: Velocity -> Float
speed (x, y) = sqrt (fromIntegral (x * x + y * y))

intersects :: Line -> Line -> Bool
intersects a b =    if d == 0
                        then False
                        else (ua >= 0 && ua <= 1 && ub >= 0 && ub <= 1)
                    where
                        ua = ((xb2 - xb1) * (ya1 - yb1) - (yb2 - yb1) * (xa1 - xb1)) / d
                        ub = ((xa2 - xa1) * (ya1 - yb1) - (ya2 - ya1) * (xa1 - xb1)) / d
                        d = (yb2 - yb1) * (xa2 - xa1) - (xb2 - xb1) * (ya2 - ya1)
                        ((xa1, ya1), (xa2, ya2)) = a
                        ((xb1, yb1), (xb2, yb2)) = b