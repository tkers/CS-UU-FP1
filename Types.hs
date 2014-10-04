module Types where

type Point = (Float,Float)
type Line = (Point,Point)

type Position = (Int,Int)
type Velocity = (Int,Int)
type CarState = (Position, Velocity)
type Trace = [Position]

type Track = (Position, Line, [Line])

-- Convert (Int, Int) to (Float, Float)
positionToPoint :: Position -> Point
positionToPoint (x, y) = (fromIntegral x, fromIntegral y)

-- functions for retrieving components of 3-tuples
first (x, _, _) = x
second (_, x, _) = x
third (_, _, x) = x