module CAM where

import Data.Int (Int32)
import Data.Word (Word8)
import GHC.Arr

{- CAM paraphernalia -}

heapSize = 20

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
type Var = String
type TaggedField = (Tag, Pat)

data Pat = PatVar Var
         | Empty
         | PatPair Pat Pat
         | As Var Pat      -- var `as` pat; equivalent to @ in Haskell
         deriving (Ord, Show, Eq, Read)


type OperationNumber = Word8

data RTS2 = SEND
          | CHOOSE
          | SPAWNEXTERNAL
          | WRAP
          deriving (Ord, Show, Eq, Read)

data RTS1 = CHANNEL | RECV
          | SPAWN   | SYNC
          deriving (Ord, Show, Eq, Read)


data Exp = Var Var  -- variable
         | Sys Sys  -- Primops
         | Void     -- Empty Tuple
         | Pair Exp Exp    -- Pair
         | Con  Tag Exp    -- Constructed Value
         | App Exp Exp     -- Function application
         | Lam Pat Exp     -- Lambda Abstraction
         | If Exp Exp Exp  -- if then else
         | Let Pat Exp Exp -- Let bindings
         | Letrec [(Pat,Exp)] Exp -- letrec
         | Case Exp [(TaggedField, Exp)]
         | Sequence Exp Exp -- ;
         deriving (Ord, Show, Eq, Read)

data Sys = Sys2 BinOp Exp Exp -- BinOp
         | Sys1 UnaryOp Exp   -- UnaryOp
         | LInt Int32   -- Int s(0) in cam
         | LFloat Float -- Float s(0) in cam
         | LBool Bool   -- Bool s(0) in cam
         | RTS2 RTS2 Exp Exp
         | RTS1 RTS1 Exp
         deriving (Ord, Show, Eq, Read)

data BinOp = PlusI | MultiplyI  | MinusI |
             PlusF | MultiplyF  | MinusF |
               BGT | BLT | BEQ  | BGE    | BLE
           deriving (Ord, Eq, Show, Read)

data UnaryOp = Abs | Neg | NOT | DEC deriving (Ord, Eq, Show, Read)

data Instruction
   = -- ACCESS INSTRUCTIONS
     FST      -- access the left component of the environement register
   | SND      -- access the right component of the environement register
   | ACC  Int -- ACC n  := FST^n;SND
   | REST Int -- REST n := FST^n

     -- STACK OPERATIONS
   | PUSH       -- copy content of register to stack
   | SWAP       -- interchange between register and topmost element of stack
   | MOVE       -- move register content to stack
   | POP        -- pop the first entry of the stack and place it on the stack

     -- REGISTER OPERATIONS
   | QUOTE Sys  -- QUOTE S(0) load immediate i.e `li` from the code area to environment
   | CLEAR      -- clear environment register
   | PRIM1 UnaryOp -- PRIM s(1) then apply primop to env reg
   | PRIM2 BinOp   -- PRIM s(2) then apply primop to top of stack (arg1) and env reg(arg2)

     -- CONTROL INSTRUCTIONS
   | SKIP   -- NoOp
   | STOP   -- halt machine
   | RETURN -- return from a subroutine call
   | CALL Label
   | GOTOFALSE Label
   | GOTO Label
   | GOTOIFALSE Label
   | SWITCHI [(Tag, Label)]

     -- Calling an RTS function
   | CALLRTS OperationNumber

   | FAIL -- a meta instruction to indicate search failure

   -- Memory growing functions
   | CONS
   | SNOC
   | CUR  Label
   | COMB Label
   -- perhaps PACK, APP and SWITCH created
   -- pairs can be stack allocated
   | PACK Tag
   | APP
   | SWITCH [(Tag, Label)]

   | CONSP
   | SNOCP
   | CURP  Label
   | COMBP Label
   deriving (Ord, Show, Eq, Read)
