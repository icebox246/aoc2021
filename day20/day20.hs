import qualified Data.Map as Map
import Data.List

type Point = (Int,Int)
type Field = Map.Map Point Char

parseFieldLines :: [String] -> Field
parseFieldLines lns = do
    let h = length lns
    let w = length $ head lns :: Int
    foldl (parseLine' w) Map.empty [0..h-1]
        where
            parseLine' :: Int -> Field -> Int -> Field
            parseLine' w mp y =
                foldl (\ m x -> Map.insert (x,y) (lns !! y !! x) m) mp [0..w-1]

generatePointsAround :: Point -> Point -> [Point]
generatePointsAround (xa,ya) (xb,yb) =
    [(x,y) |  y <- [ya-1..yb+1], x <- [xa-1..xb+1]]

applyAlgorithm :: String -> Bool -> Field  -> Field 
applyAlgorithm algo evn startField = do
    let minPt = fst . head $ Map.toList startField
    let maxPt = fst . last $ Map.toList startField
    let newFieldPoints = generatePointsAround minPt maxPt
    foldl (\mp pt -> Map.insert pt (processPoint pt algo evn startField) mp) Map.empty newFieldPoints

binArrToInt :: [Int] -> Int
binArrToInt = foldl (\acc d -> acc * 2 + d) 0

processPoint :: Point -> String -> Bool -> Field -> Char
processPoint pt algo evn field = do
    let surroundings = generatePointsAround pt pt
    let idx = binArrToInt $ map (\p -> case field Map.!? p of
                                         Nothing -> if evn && head algo == '#' then 1 else 0
                                         Just '.' -> 0
                                         Just '#' -> 1
                                         _ -> error "Unknown state!") surroundings
    if idx >= 512 then error $ "wtf: " ++ show idx
                  else algo !! idx

stringifyField :: Field -> String
stringifyField field = do
    let minPt = fst . head $ Map.toList field
    let maxPt = fst . last $ Map.toList field
    unlines [ [field Map.! (x,y) | x <- [fst minPt..fst maxPt] ] | y <- [snd minPt..snd maxPt]]
    



main :: IO ()
main = do
    input <- getContents
    let (algorithm:_:fieldLines) = lines input
    let field = parseFieldLines fieldLines


    putStrLn "Part I:"
    print $ length $ filter (=='#') $ map snd $ Map.toList $ applyAlgorithm algorithm True $ applyAlgorithm algorithm False field 

    putStrLn "Part II:"
    print $ length $ filter (=='#') $ map snd $ Map.toList $ foldl (\f i -> applyAlgorithm algorithm (even i) f) field [1..50]
