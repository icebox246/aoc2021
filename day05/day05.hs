{-# LANGUAGE TupleSections #-}

import Data.List

toInt :: String -> Int
toInt s = read s :: Int

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn el arr = splitOn' el arr []
    where
        splitOn' :: Eq a => a -> [a] -> [a] -> [[a]]
        splitOn' _ [] buf = [buf]
        splitOn' el (a:as) buf
            | a == el && buf /= [] = buf: splitOn' el as []
            | a == el && null buf = splitOn' el as []
            | otherwise = splitOn' el as (buf ++ [a])

count :: Eq a => (a -> Bool) -> [a] -> Int
count fn xs = cnt' fn xs 0
    where
        cnt' _ [] c = c
        cnt' fn (x:xs) c
            | fn x = cnt' fn xs (c+1)
            | otherwise = cnt' fn xs c


type Point = (Int,Int)
type Vent = (Point,Point)

parsePoint :: String -> Point
parsePoint str = do
    let [x,y] = map toInt $ splitOn ',' str
    (x,y)

parseVent :: String -> Vent
parseVent line = do
    let [p1,_,p2] = words line
    (parsePoint p1, parsePoint p2)

isVentStraight :: Vent -> Bool
isVentStraight v = do
     let ((x1,y1),(x2,y2)) = v
     x1 == x2 || y1 == y2

pointCmp :: Point -> Point -> Ordering
pointCmp (x1,y1) (x2,y2)
  | x1 < x2 = LT
  | x1 == x2 && y1 < y2 = LT
  | otherwise = GT


getVentPoints :: Vent -> [Point]
getVentPoints ((x1,y1),(x2,y2))
  | x1 == x2 = map (x1,) [min y1 y2..max y1 y2]
  | y1 == y2 = map (,y1) [min x1 x2..max x1 x2]
  | otherwise = do
      let [(x1',y1'),(x2',y2')] = sortBy pointCmp [(x1,y1),(x2,y2)]
      let negY = if y1' <= y2' then 1 else -1
      let dx = x2' - x1'
      map (\o -> (x1' + o, y1' + o * negY)) [0..dx]


countReps :: Eq a => [a] -> [(a,Int)]
countReps [] = []
countReps (x:xs) = cr' xs x 1
    where
        cr' [] l c = [(l,c)]
        cr' (x:xs) l c
            | x == l = cr' xs l (c+1)
            | otherwise = (l,c) : cr' xs x 1

main = do
    input <- getContents
    let inputLines = lines input
    let vents = map parseVent inputLines
    let straightVents = filter isVentStraight vents

    let ventPoints1 = sortBy pointCmp $ foldl1 (++) $ map getVentPoints straightVents
    let overlaps1 = count (\(_,c) -> c >= 2) $ countReps ventPoints1

    putStrLn "Part I:"
    print overlaps1

    let ventPoints2 =  sortBy pointCmp $ foldl1 (++) $ map getVentPoints vents
    let overlaps2 = count (\(_,c) -> c >= 2) $ countReps ventPoints2

    putStrLn "Part II:"
    print overlaps2

    putStrLn "Nice!"
