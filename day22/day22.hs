{-# LANGUAGE TupleSections #-}
import Data.Maybe
import qualified Data.Map as Map
data Operation = Opr Int Range Range Range
    deriving (Show, Eq)
type Range = (Int,Int)
type Grid  = Map.Map (Int,Int,Int) Int

type Point = (Int,Int,Int)
type Cuboid = ((Int,Int,Int), (Int,Int,Int))

cubFromRanges :: (Range,Range,Range) -> Cuboid
cubFromRanges ((x1,x2),(y1,y2),(z1,z2)) = ((x1,y1,z1),(x2,y2,z2))

cubVolume :: Cuboid -> Int
cubVolume ((x1,y1,z1),(x2,y2,z2)) = (x2 - x1 + 1) * (y2 - y1 + 1) * (z2 - z1 + 1)

intersectCub :: Cuboid -> Cuboid -> Maybe Cuboid
intersectCub ((ax1,ay1,az1),(ax2,ay2,az2)) ((bx1,by1,bz1),(bx2,by2,bz2)) = do
    let x1 = max ax1 bx1
    let y1 = max ay1 by1
    let z1 = max az1 bz1
    let x2 = min ax2 bx2
    let y2 = min ay2 by2
    let z2 = min az2 bz2
    if x2 - x1 < 0 || y2 - y1 < 0 || z2 - z1 < 0 then Nothing
                                                    else return ((x1,y1,z1),(x2,y2,z2))

parseInt :: String -> Int
parseInt s = read s :: Int

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn el xs = takeWhile (/=el) xs : splitOn el (dropWhile (==el) $ dropWhile (/=el) xs)

parseRange :: String -> Range
parseRange s = let [mi,mx] = map parseInt $ splitOn '.' s in (mi,mx)


parseOperation :: String -> Operation
parseOperation str = do
    let [opType, rangeS] = words str
    let [x,y,z] = map (parseRange . drop 2) $ splitOn ',' rangeS
    case opType of
      "on" -> Opr 1 x y z
      "off" -> Opr 0 x y z
      _ -> error "Unknown operation!"


clampRange :: Range -> Range -> Maybe Range
clampRange (t1,t2) (o1,o2) =
    if o1 > t2 || o2 < t1 then Nothing
                          else return (max t1 o1,min t2 o2)

clampOperation :: (Range, Range, Range) -> Operation -> Maybe Operation
clampOperation (tx,ty,tz) (Opr v ox oy oz) =
    let clamped = (clampRange tx ox, clampRange ty oy, clampRange tz oz)
    in case clamped of
      (Just x, Just y, Just z) -> return $ Opr v x y z
      _ -> Nothing

ranges1 = ((-50,50),(-50,50),(-50,50))

applyOp2 :: Operation -> [(Cuboid,Int)] -> [(Cuboid,Int)]
applyOp2 (Opr v rx ry rz) cubList = do
    let cub = cubFromRanges (rx,ry,rz)
    let addedCubs = 
            mapMaybe (\(c,s) -> do
                let sect = intersectCub c cub
                case sect of 
                  Nothing -> Nothing
                  Just inter -> return (inter,-s)
                ) cubList
    let cubList' = cubList ++ addedCubs
    if v == 1 then (cub,v) : cubList'
              else cubList'

main :: IO ()
main = do
    input <- getContents
    let operations = map parseOperation $ lines input
    let clampedOperations = mapMaybe (clampOperation ranges1) operations

    let finalGrid1 = foldl (flip applyOp2) [] clampedOperations

    putStrLn "Part I:"
    print $ foldl (\acc (cub,sig) -> acc + sig * cubVolume cub) 0 finalGrid1

    let finalGrid2 = foldl (flip applyOp2) [] operations

    putStrLn "Part II:"
    print $ foldl (\acc (cub,sig) -> acc + sig * cubVolume cub) 0 finalGrid2
