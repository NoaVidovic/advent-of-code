module Day7 where


data Kind
    = File Int
    | Dir [Entry]
    deriving (Show, Eq)


data Entry =
    Entry
        { name :: String
        , kind :: Kind
        }
    deriving (Show, Eq)


newtype Path = Path [String]
    deriving (Show, Eq)


getPath :: Path -> [String]
getPath path =
    case path of Path p -> p


parseLine :: String -> Entry
parseLine line =
    if t == "dir" then
        Entry name $ Dir []
    else
        Entry name $ File (read t :: Int)
    where
        (t, name_) = break (== ' ') line
        name = tail name_


parseDirectory :: [String] -> Entry
parseDirectory lines =
    Entry rootName $ Dir $ map parseLine contents
        where
            rootName = drop (length "$ cd ") $ head lines
            contents = tail $ tail lines


cd :: String -> Path -> Path
cd arg cwd =
    if arg == ".." then
        Path $ reverse $ tail $ reverse $ getPath cwd
    else
        Path $ reverse $ arg : (reverse $ getPath cwd)


get :: String -> Entry -> Entry
get searchName dir =
    case kind dir of
      File _      -> undefined
      Dir entries -> head $ filter (\x -> name x == searchName) entries


accessPath :: Path -> Entry -> Entry
accessPath path dir =
    foldl (flip get) dir $ getPath path


removeEntry :: String -> [Entry] -> [Entry]
removeEntry s =
    filter (\x -> name x /= s)


addToPath :: Path -> Entry -> Entry -> Entry
addToPath path thing dir =
    case kind dir of
      File _       -> undefined
      Dir contents ->
        case getPath path of
          [] ->
            Entry (name dir) $ Dir $ thing : contents
          h:t ->
            Entry (name dir) $ Dir $ (addToPath (Path t) thing $ get h dir) : (removeEntry h contents)


removePath :: Path -> Entry -> Entry
removePath path dir =
    case kind dir of
      File _ -> undefined
      Dir contents ->
          case getPath path of
            []   -> dir
            h:[] -> Entry (name dir) $ Dir $ removeEntry h contents
            h:t  -> Entry (name dir) $ Dir $ (removePath (Path t) $ get h dir) : (removeEntry h contents)


applyInputs :: [String] -> (Path, Entry) -> (Path, Entry)
applyInputs contents (cwd, rootDir) =
    case contents of
      []          -> (cwd, rootDir)
      ["$ cd .."] -> (cd ".." cwd, rootDir)
      _           ->
          (newPath, addToPath cwd newDir $ removePath newPath rootDir)
              where
                  newDir = parseDirectory contents
                  newPath = cd (name newDir) cwd


splitInput :: [String] -> [[String]]
splitInput input@[] = []
splitInput input@(cdLine:rest) =
    (cdLine : a) : (splitInput b)
        where
            (a, b) = break (\x -> "$ cd " == take 5 x) rest


flatten :: Entry -> [Entry]
flatten entry@(Entry _ k) =
    case k of
      File _       -> [entry]
      Dir contents -> entry : (concat $ map flatten contents)


size :: Entry -> Int
size entry@(Entry _ k) =
    case k of
      File s       -> s
      Dir contents -> sum $ map size contents


dirSizes :: String -> [Int]
dirSizes input =
    map size $ filter isDir $ flatten root
        where
            root = snd $ foldl (flip applyInputs) (Path [], parseDirectory $ head inputs) (tail inputs)
            inputs = splitInput $ lines input
            isDir e@(Entry _ k) =
                case k of
                  File _ -> False
                  Dir _  -> True


solve :: String -> Int
solve input =
    sum $ filter (<= 100000) $ dirSizes input


solve2 :: String -> Int
solve2 input =
    minimum $ filter (> toBeCleared) $ dirSizes input
        where
            toBeCleared = (unusedSpaceRequiredForUpdate -) $ (totalSpace -) $ maximum $ dirSizes input


totalSpace :: Int
totalSpace = 70000000


unusedSpaceRequiredForUpdate :: Int
unusedSpaceRequiredForUpdate = 30000000


main = do
    input <- readFile "Day7.txt"
    putStrLn $ show $ solve input
    putStrLn $ show $ solve2 input
