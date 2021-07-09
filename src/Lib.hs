module Lib
    ( visualise
    ) where

import Data.Int (Int32)
import Graphics.Gloss
import GHC.Arr
import GHC.Float
import qualified Data.Map as Map

{-
[(1,<(l:1,l:dummy)>),(2,<(3,2)>),(3,<(*null,*4)>),(4,<(*null,*5)>),(5,<(*null,*6)>),
(6,<(*null,*7)>),(7,<(*null,*8)>),(8,<(*null,*9)>),(9,<(*null,*10)>),(10,<(*null,*11)>),
(11,<(*null,*12)>),(12,<(*null,*13)>),(13,<(*null,*14)>),(14,<(*null,*15)>),(15,<(*null,*16)>),
(16,<(*null,*17)>),(17,<(*null,*18)>),(18,<(*null,*19)>),(19,<(*null,*20)>),(20,<(*null,*null)>)]

-}


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

heapSize = 20

-- Interpret 2 bytecodes per second
speedFactor = 2

buildHeap :: Heap -> Picture
buildHeap h = pictures $ buildHeap' (assocs h) startFromY

buildHeap' :: [(Int, HeapCell)] -> Float -> [Picture]
buildHeap' [] _ = []
buildHeap' ((i,hc):h) y
  | imod5 == 0 && (i' /= 0) =
    (drawCell hc startFromX (y - vgap)) : buildHeap' h (y - vgap)
  | otherwise =
    (drawCell hc (startFromX + imod5  * hgap) y) : buildHeap' h y
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
  let programTransitions = read x :: [Heap]
  let m = Map.fromList $ zip [1..(length programTransitions)] programTransitions
  animate disp white (drawPicture m)

drawPicture :: Map.Map Int Heap -> Float -> Picture
drawPicture m i
  | i < 1 = buildHeap initHeap
  | otherwise =
    case Map.lookup (floor (i*speedFactor)) m of
      Nothing -> finalHeapPic
      Just h  -> buildHeap h
  where
    finalHeapPic =
        case Map.lookup 104 m of
          Nothing -> buildHeap initHeap
          Just h  -> buildHeap h



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



{- CAM paraphernalia -}

type Heap  = Array Int HeapCell

data HeapCell = HeapCell (CellContent, CellContent)
              deriving (Eq, Show, Read)


type Pointer = Int

data CellContent = V Val
                 | P Pointer
                 | L Label
                 | T Tag
                 deriving (Eq, Show, Read)

data Val = VInt  Int32   -- constants s(0)
         | VFloat Float  -- constants s(0)
         | VBool Bool    -- constants s(0)
         | VEmpty        -- empty tuple
         deriving (Ord, Eq, Read)

instance Show Val where
  show (VInt i)   = show i
  show (VFloat f) = show f
  show (VBool b)  = show b
  show  VEmpty    = "()"

type Tag = String

newtype Label =
  Label { getLabel :: Int }
  deriving (Ord, Eq, Read)

instance Show Label where
  show (Label i) = show i

nullPointer = (-1)
emptyCell   = HeapCell (P nullPointer, P nullPointer)

initHeap :: Heap
initHeap = listArray (1, heapSize) finalHeap
  where
    nullHeap = replicate heapSize emptyCell
    tempHeap =
      zipWith (\i (HeapCell (P x, _)) -> HeapCell (P x, P i))
      (take heapSize [2..]) nullHeap
    (restcells, _) = splitAt (heapSize - 1) tempHeap
    finalHeap = restcells ++ [emptyCell]

cyclicHeap :: Heap
cyclicHeap = listArray (1, 4) finalHeap
  where
    finalHeap = [ HeapCell (V (VInt 5), P 2)
                , HeapCell (V (VInt 3), P 1)
                , HeapCell (P nullPointer, P 4) --freeList
                , emptyCell
                ]
