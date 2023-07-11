{-# LANGUAGE TypeApplications #-}

module Day10 where

data Op
  = Noop
  | Addx Int

data State = State Cycle Value
    deriving Show

type Cycle = Int

type Value = Int

cyclesOf :: Op -> Cycle
cyclesOf Noop = 1
cyclesOf (Addx _) = 2

valueOf :: Op -> Value
valueOf Noop = 0
valueOf (Addx i) = i

parseLine :: String -> Op
parseLine s =
  case take 4 s of
    "noop" -> Noop
    "addx" -> Addx (read @Int $ drop 4 s)

applyOp :: Op -> State -> State
applyOp op s@(State c v) =
  State (c + cyclesOf op) (valueOf op)

initState :: State
initState = State 0 1

-- solve :: String -> Int
solve =
    foldl (flip applyOp) initState . map parseLine . lines

main :: IO ()
main = do
  input <- readFile "Day10.txt"
  print $ solve input
