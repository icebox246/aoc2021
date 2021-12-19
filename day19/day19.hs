import Data.List
import Data.Maybe
import Data.Bifunctor
import qualified Data.Map as M

type Scanner = [Vec]
type Vec = (Int,Int,Int)

strToInt s = read s :: Int

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn el xs = split' [] xs
    where
        split' buf [] = [buf]
        split' buf (x:xs)
            | x == el = buf : split' [] xs
            | otherwise = split' (buf ++ [x]) xs

parseBeacon :: String -> Vec
parseBeacon s = do
    let ns = splitOn ',' s
    case ns of
      [a,b,c] -> (strToInt a, strToInt b, strToInt c)
      _       -> error "Invalid beacon format!"

rotations = [
                \(x,y,z) -> ( x, y, z),   -- +x, +y
                \(x,y,z) -> ( x,-z, y),   -- +x, +z
                \(x,y,z) -> ( x,-y,-z),   -- +x, -y
                \(x,y,z) -> ( x, z,-y),   -- +x, -z

                \(x,y,z) -> (-z, y, x),   -- +z, +y
                \(x,y,z) -> (-y,-z, x),   -- +z, -x
                \(x,y,z) -> ( z,-y, x),   -- +z, -y
                \(x,y,z) -> ( y, z, x),   -- +z, +x

                \(x,y,z) -> (-y, x, z),   -- +y, -x
                \(x,y,z) -> (-z, x,-y),   -- +y, -z
                \(x,y,z) -> ( y, x,-z),   -- +y, +x
                \(x,y,z) -> ( z, x, y),   -- +y, +z

                \(x,y,z) -> (-x, y,-z),   -- -x, +y
                \(x,y,z) -> (-x, z, y),   -- -x, +z
                \(x,y,z) -> (-x,-y, z),   -- -x, -y
                \(x,y,z) -> (-x,-z,-y),   -- -x, -z

                \(x,y,z) -> ( z, y,-x),   -- -z, +y
                \(x,y,z) -> (-y, z,-x),   -- -z, -x
                \(x,y,z) -> (-z,-y,-x),   -- -z, -y
                \(x,y,z) -> ( y,-z,-x),   -- -z, +x

                \(x,y,z) -> (-y,-x,-z),   -- -y, -x
                \(x,y,z) -> ( z,-x,-y),   -- -y, -z
                \(x,y,z) -> ( y,-x, z),   -- -y, +x
                \(x,y,z) -> (-z,-x, y)    -- -y, +z
            ] :: [Vec -> Vec]

generateRotations :: Scanner -> [Scanner]
generateRotations scn = map (`map` scn) rotations


sqrDist :: Vec -> Vec -> Int
sqrDist (x1,y1,z1) (x2,y2,z2) =
    (x1-x2) ^ 2 +
    (y1-y2) ^ 2 +
    (z1-z2) ^ 2

(...-) :: Vec -> Vec -> Vec
(x1,y1,z1) ...- (x2,y2,z2) = (x1-x2,y1-y2,z1-z2)

(...+) :: Vec -> Vec -> Vec
(x1,y1,z1) ...+ (x2,y2,z2) = (x1+x2,y1+y2,z1+z2)

allEqual :: Eq a => [a] -> Bool
allEqual [] = error "Empty list!"
allEqual [x] = True
allEqual (x:xs) = all (==x) xs

has12Equal :: Ord a => [a] -> Maybe a
has12Equal arr = let (x:xs) = sort arr in chk' 1 x xs
    where
        chk' _ _ [] = Nothing
        chk' rc el (x:xs)
            | el == x = if rc == 11 then return el else  chk' (rc+1) el xs
            | otherwise = chk' 1 x xs

findMatch :: Int -> [Scanner] -> [Relation]
findMatch i scanners = do
    let myBeacs = scanners !! i :: Scanner
    let rotCnt = length rotations
    let scannerCnt = length scanners

    map (\((x1,x2):_,y) -> (y,x2,x1)) $ filter (not . null . fst) [ (cmp myBeacs (scanners !! oi),oi) | oi <- [0..scannerCnt-1], oi /= i ]
        where
            cmp :: Scanner -> Scanner -> [(Vec,Int)]
            cmp b1s b2s = do
                let rots = generateRotations b2s
                let rotCnt = length rotations
                map (first fromJust) $ filter (isJust . fst) [(has12Equal [ b1 ...- b2 | b1 <- b1s, b2 <- rots !! ri ],ri) | ri <- [0..rotCnt-1]]

applyRotQ :: Vec -> [Int] -> Vec
applyRotQ = foldl (\v' r -> (rotations !! r) v')

type ScannerTrans = (Vec,[Int])
type Relation = (Int,Int,Vec)

solveRelations :: M.Map Int ScannerTrans -> M.Map Int [Relation] -> M.Map Int ScannerTrans
solveRelations transs relations = do
    let newTranss = foldl check1 transs $ M.toList relations
    if newTranss == transs then newTranss
                           else solveRelations newTranss relations
        where
            check1 :: M.Map Int ScannerTrans -> (Int,[Relation]) -> M.Map Int ScannerTrans
            check1 oldTransMap (rtId,rtRels) =
                case oldTransMap M.!? rtId of
                  Nothing -> oldTransMap
                  Just rtTrans@(rtPos,rtRots) ->
                      foldl (\mp rel@(i,r,off) -> M.insert i (rtPos ...+ applyRotQ off rtRots, r:rtRots) mp) oldTransMap $ filter (\(i, _, _) -> isNothing (oldTransMap M.!? i)) rtRels

resolveBeaconPositions :: ScannerTrans -> [Vec] -> [Vec]
resolveBeaconPositions (rtPos,rtRots) = map (\off -> rtPos ...+ applyRotQ off rtRots)

uniq :: Ord a => [a] -> [a]
uniq [] = []
uniq arr = let (x:xs) = sort arr in x : un' x xs
    where
        un' _ [] = []
        un' el (x:xs)
            | x == el = un' el xs
            | otherwise = x : un' x xs

manhatan :: Vec -> Vec -> Int
manhatan v1 v2 = do
    let (x,y,z) = v1 ...- v2
    abs x + abs y + abs z

main = do
    input <- getContents
    let scanners = map (map parseBeacon . drop 1) $ splitOn "" $ lines input :: [Scanner]
    let scannerCnt = length scanners
    -- print (head scanners)
    -- putStrLn $ unlines $ map show $ generateRotations $ scanners !! 1

    let relations = M.fromList [(scnId,findMatch scnId scanners) | scnId <- [0..scannerCnt-1]]
    -- putStrLn  $ unlines $ map show $ M.toList relations
    let scan0Trans = ((0,0,0),[]) :: ScannerTrans
    let scannerTransforms =  solveRelations (M.fromList [(0,scan0Trans)]) relations
    -- putStrLn  $ unlines $ map show $ M.toList scannerTransforms

    let scannerAndBeacons = zip (map snd $ M.toList scannerTransforms) scanners
    -- print $ sort $ resolveBeaconPositions (scannerTransforms M.! 0) (head scanners)

    let beacons = uniq $ concatMap (uncurry resolveBeaconPositions) scannerAndBeacons

    putStrLn "Part I:"
    print $ length beacons

    putStrLn "Part II:"
    print $ maximum [ manhatan p1 p2 | (_,(p1,_)) <- M.toList scannerTransforms, (_,(p2,_)) <- M.toList scannerTransforms ]



