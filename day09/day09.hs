{-# LANGUAGE TupleSections #-}
import           Data.List

digit :: Char -> Int
digit c = read [c] :: Int

count :: (a -> Bool) -> [a] -> Int
count _ [] = 0
count fn (x:xs)
    | fn x = 1 + count fn xs
    | otherwise = count fn xs

inRange :: Int -> Int -> Int -> Bool
inRange v a b = v >= a && v <= b

type Point = (Int,Int)

getVal :: Point -> [[Int]] -> Int
getVal (x,y) grid
  | x >= 0 && x < length (head grid) && y >= 0 && y < length grid = grid !! y !! x
  | otherwise = 100000000

setVal :: Point -> [[Int]] -> Int -> [[Int]]
setVal (x,y) grid v
  | x >= 0 && x < length (head grid) && y >= 0 && y < length grid = do
      let ln = grid !! y
      let nl = take x ln ++ [v] ++ drop (x+1) ln
      take y grid ++ [nl] ++ drop (y+1) grid
  | otherwise = grid

checkLow :: Point -> [[Int]] -> Bool
checkLow p@(x,y) grid = do
    let cval = getVal p grid :: Int
    all (>cval) [
                    getVal (x-1,y) grid,
                    getVal (x+1,y) grid,
                    getVal (x,y-1) grid,
                    getVal (x,y+1) grid
                ]

climbBasin :: Point -> [[Int]] -> Int
climbBasin p grid  = count (==(-1)) $ foldl1 (++) $ climbBasin' p grid
    where
        climbBasin' :: Point -> [[Int]] -> [[Int]]
        climbBasin' p@(x,y) grid = do
            let cval = getVal p grid 
            let gr = setVal p grid (-1)
            let g1 = if inRange (getVal (x-1,y) gr) cval 8 then climbBasin' (x-1,y) gr else gr
            let g2 = if inRange (getVal (x+1,y) g1) cval 8 then climbBasin' (x+1,y) g1 else g1
            let g3 = if inRange (getVal (x,y-1) g2) cval 8 then climbBasin' (x,y-1) g2 else g2
            let g4 = if inRange (getVal (x,y+1) g3) cval 8 then climbBasin' (x,y+1) g3 else g3
            g4

main = do
    input <- getContents
    let grid = map (map digit) $ lines input :: [[Int]]
    let height = length grid
    let width = length $ head grid

    let points = foldl1 (++) $ map (\y -> map (,y) [0..(width -1)]) [0..(height-1)]

    let lowPoints = filter (`checkLow` grid) points
    let lows = map (`getVal` grid)  lowPoints

    putStrLn "Part I:"
    print $ sum $ map (+1) lows

    let basins = map (`climbBasin` grid) lowPoints


    putStrLn "Part II:"
    print . product . take 3 . reverse . sort $ basins



