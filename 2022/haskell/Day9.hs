module Day9 where


import Prelude hiding (Left, Right)

import Data.List (nub)


data Step
    = Left Int
    | Right Int
    | Up Int
    | Down Int


type Pos = (Int, Int)


deconstruct :: Step -> (Int -> Step, Int)
deconstruct s =
    case s of
      Left x  -> (Left, x)
      Right x -> (Right, x)
      Up x    -> (Up, x)
      Down x  -> (Down, x)


parseLine :: String -> Step
parseLine s@""     = undefined
parseLine s@(h:t)  =
    case h of
      'L' -> Left $ read t
      'R' -> Right $ read t
      'U' -> Up $ read t
      'D' -> Down $ read t


moveHead1 :: Step -> Pos -> Pos
moveHead1 s (hx, hy) =
    case s of
      Left _  -> (hx - 1, hy)
      Right _ -> (hx + 1, hy)
      Up _    -> (hx, hy + 1)
      Down _  -> (hx, hy - 1)


moveTail :: Step -> Pos -> Pos -> Pos
moveTail s (hx, hy) (tx, ty)
    | dx + dy > 2 = (tx + sx, ty + sy)
    | dx > 1      = (tx + sx, ty)
    | dy > 1      = (tx, ty + sy)
    | otherwise   = (tx, ty)
        where
            sx = signum $ hx - tx
            sy = signum $ hy - ty
            dx = abs $ hx - tx
            dy = abs $ hy - ty


moveTail2 :: Step -> Pos -> [Pos] -> [Pos]
moveTail2 s h [] = []
moveTail2 s h t =
    movedHead : moveTail2 s movedHead (tail t)
        where
            movedHead = moveTail s h $ head t


recordPos :: Step -> ((Pos, Pos), [Pos]) -> ((Pos, Pos), [Pos])
recordPos s ((head, tail), r) =
    case deconstruct s of
      (_, 0)  -> ((head, tail), r)
      (d, x)  -> recordPos (d $ x-1) ((movedHead, movedTail), movedTail:r)
          where
              movedHead = moveHead1 s head
              movedTail = moveTail s movedHead tail


recordPos2 :: Step -> ([Pos], [Pos]) -> ([Pos], [Pos])
recordPos2 s (l@(h:rest), r) =
    case deconstruct s of
      (_, 0)  -> (l, r)
      (d, x)  -> recordPos2 (d $ x - 1) (movedHead:movedTail, endOfTail:r)
          where
              movedHead = moveHead1 s h
              movedTail = moveTail2 s movedHead rest
              endOfTail = last movedTail


init1 :: ((Pos, Pos), [Pos])
init1 = (((0,0), (0,0)), [])


init2 :: ([Pos], [Pos])
init2 = (replicate 10 (0, 0), [])


solve :: String -> Int
solve input =
    length $ nub $ snd $ foldl (flip recordPos) init1 $ map parseLine $ lines input


solve2 :: String -> Int
solve2 input =
    length $ nub $ snd $ foldl (flip recordPos2) init2 $ map parseLine $ lines input

 
main = do
    input <- readFile "Day9.txt"
    print $ solve input
    print $ solve2 input
