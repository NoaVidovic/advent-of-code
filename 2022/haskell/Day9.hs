module Day9 where


data Step
    = Left Int
    | Right Int
    | Up Int
    | Down Int


type Position = (Int, Int)


parseLine :: String -> Step
parseLine s@""     = undefined
parseLine s@(h:t)  =
    case h of
      'L' -> Left $ read t
      'R' -> Right $ read t
      'U' -> Up $ read t
      'D' -> Down $ read t


main = do
    input <- readFile "Day9.txt"
    putStrLn $ show $ solve input
