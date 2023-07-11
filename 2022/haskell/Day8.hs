module Day8 where


import Data.List (transpose)


type Map = [[Char]]
type Row = [Char]


left :: Int -> Int -> Map -> Row
left i j m =
    reverse $ map snd $ filter ((< j) . fst) $ zip [0..] $ m !! i


right :: Int -> Int -> Map -> Row
right i j m =
    map snd $ filter ((> j) . fst) $ zip [0..] $ m !! i


above :: Int -> Int -> Map -> Row
above i j m =
    left j i $ transpose m


below :: Int -> Int -> Map -> Row
below i j m =
    right j i $ transpose m


isVisibleP :: (Int -> Int -> Map -> Row) -> Int -> Int -> Map -> Bool
isVisibleP p i j m =
    all (< m !! i !! j) $ p i j m


isVisibleAny :: Int -> Int -> Map -> Bool
isVisibleAny i j m =
    or $ map (\x -> isVisibleP x i j m) [ left, right, above, below ]


viewingDistanceP :: (Int -> Int -> Map -> Row) -> Int -> Int -> Map -> Int
viewingDistanceP p i j m =
    (+ add) $ length $ shorter
        where
            (shorter, taller) = break (>= m !! i !! j) $ p i j m
            add = if taller == "" then 0 else 1  -- edge case doesn't count edge


scenicScore :: Int -> Int -> Map -> Int
scenicScore i j m =
    product $ map (\x -> viewingDistanceP x i j m) [ left, right, above, below ]


solve :: String -> Int
solve input =
    length $ filter id $ map ($ m) gridToV
        where
            gridToV = concatMap (`map` [0..jMax]) $ map isVisibleAny [0..iMax]
            m = lines input
            iMax = length m - 1
            jMax = length (m !! 0) - 1


solve2 :: String -> Int
solve2 input =
    maximum $ map ($ m) gridToS
        where
            gridToS = concatMap (`map` [0..jMax]) $ map scenicScore [0..iMax]
            m = lines input
            iMax = length m - 1
            jMax = length (head m) - 1
    

main :: IO ()
main = do
    input <- readFile "Day8.txt"
    putStrLn $ show $ solve input
    putStrLn $ show $ solve2 input
