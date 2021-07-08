module Lib
    ( visualise
    ) where


import Graphics.Gloss



disp :: Display
disp = (InWindow "Heap Visualisation" (200, 200) (10, 10))


onepixel :: Picture
onepixel = rectangleSolid 20 20

redpixel :: Picture
redpixel = color red' onepixel

purplepixel :: Picture
purplepixel = color purple onepixel

yellowpixel :: Picture
yellowpixel = color yellow' onepixel

greenpixel :: Picture
greenpixel = color green' onepixel

inuse :: Float -> Float -> Picture
inuse x y = translate x y redpixel

free :: Float -> Float -> Picture
free x y = translate x y purplepixel

rect1 :: Picture
rect1 = pictures [inuse 0 0, free 50 0]

rect2 :: Picture
rect2 = pictures [inuse 0 30, free 50 30]

visualise :: IO ()
visualise = animate disp white drawPicture

drawPicture :: Float -> Picture
drawPicture i
  | i < 3 = rect1
  | otherwise = rect2




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

