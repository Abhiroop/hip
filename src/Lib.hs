module Lib
    ( visualise
    ) where

import Graphics.Gloss
import GHC.Arr
import GHC.Float
import qualified Data.Map as Map

import CAM


disp :: Display
disp = (InWindow "Heap Visualisation" (500, 500) (10, 10))


onecell :: Picture
onecell = rectangleSolid 50 50

redcell :: Picture
redcell = color red' onecell

purplecell :: Picture
purplecell = color purple onecell

yellowcell :: Picture
yellowcell = color yellow' onecell

greencell :: Picture
greencell = color green' onecell

inuse :: Float -> Float -> Picture
inuse x y = translate x y redcell

free :: Float -> Float -> Picture
free x y = translate x y purplecell

startFromX = (-150)
startFromY = 150

hgap = 80
vgap = 80



-- Interpret 2 bytecodes per second
speedFactor = 2

bytecode :: String -> Picture
bytecode = translate (-30) (-180) . scale 0.2 0.2 . text

buildHeap :: Instruction -> Heap -> Picture
buildHeap i h = pictures $ buildHeap' i (assocs h) startFromY

buildHeap' :: Instruction -> [(Int, HeapCell)] -> Float -> [Picture]
buildHeap' inst [] _ = [(bytecode $ show inst)]
buildHeap' inst ((i,hc):h) y
  | imod5 == 0 && (i' /= 0) =
    (drawCell hc startFromX (y - vgap)) : buildHeap' inst h (y - vgap)
  | otherwise =
    (drawCell hc (startFromX + imod5  * hgap) y) : buildHeap' inst h y
  where
    imod5 = int2Float $ i' `mod` 5
    i' = i - 1


drawCell :: HeapCell -> Float -> Float -> Picture
drawCell h x y
  | isFree h  = free x y
  | otherwise = inuse x y

isFree :: HeapCell -> Bool
isFree (HeapCell (P (-1), _)) = True
isFree _ = False

visualise :: IO ()
visualise = do
  x <- readFile "foo.txt"
  let programTransitions = read x :: [(Instruction, Heap)]
  let m = Map.fromList $ zip [1..(length programTransitions)] programTransitions
  animate disp white (drawPicture m)



-- baz = do
--   x <- readFile "foo.txt"
--   let programTransitions = read x :: [(Instruction,Heap)]
--   pure $ length programTransitions

drawPicture :: Map.Map Int (Instruction, Heap) -> Float -> Picture
drawPicture m i
  | i < 1 = buildHeap STOP initHeap
  | otherwise =
    case Map.lookup (floor (i*speedFactor)) m of
      Nothing -> finalHeapPic
      Just (i, h)  -> buildHeap i h
  where
    finalHeapPic =
        case Map.lookup 34 m of
          Nothing -> buildHeap FAIL initHeap
          Just (i, h)  -> buildHeap i h



-- Colors --

purple :: Color
purple = makeColor 128 0 128 0.25

red' :: Color
red' = makeColor 255 0 0 0.5

yellow' :: Color
yellow' = makeColor 255 247 0 0.85

green' :: Color
green' = makeColor 0 255 0 0.85


-- Shapes --

triangle :: Picture
triangle = polygon path
  where
    path = [(0,0), (10,0), (10,10)]

rect1 :: Picture
rect1 = pictures [inuse (-150) 150, free 30 0]

rect2 :: Picture
rect2 = pictures [inuse (-80) 30, free 30 30]



