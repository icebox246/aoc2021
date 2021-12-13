import           Data.List
import           Data.Maybe
type Point = (Int,Int)
data Fold = OnX Int | OnY Int
    deriving (Show,Eq)

strToInt :: String -> Int
strToInt s = read s :: Int

splitAtFirst :: Eq a => a -> [a] -> [[a]]
splitAtFirst = split' []
    where
        split' buf _ [] = [buf]
        split' buf el (x:xs)
            | x == el = [buf,xs]
            | otherwise = split' (buf ++ [x]) el xs

parsePoint :: String -> Point
parsePoint s = let [x,y] = map strToInt $ splitAtFirst ',' s in (x,y)

parseOp :: String -> Fold
parseOp s = do
    let [axis,sval] = splitAtFirst '=' $ drop (length "fold along ") s
    let val = strToInt sval
    case axis of
        "x" -> OnX val
        "y" -> OnY val
        _   -> error $ "Wrong axis: " ++ axis


applyFoldOnPoint :: Fold -> Point -> Maybe Point
applyFoldOnPoint (OnX fx) p@(x,y)
  | x == fx = Nothing
  | x > fx = Just (fx + fx - x,y)
  | otherwise = Just p
applyFoldOnPoint (OnY fy) p@(x,y)
  | y == fy = Nothing
  | y > fy = Just (x,fy + fy - y)
  | otherwise = Just p

removeDups :: Ord a => [a] -> [a]
removeDups arr = let (x:xs) = sort arr in trav' x xs
    where
        trav' :: Eq a => a -> [a] -> [a]
        trav' el [] = [el]
        trav' el (x:xs)
          | el == x = trav' el xs
          | otherwise = el : trav' x xs

applyFold :: Fold -> [Point] -> [Point]
applyFold f = removeDups . mapMaybe (f `applyFoldOnPoint`)

findMinMax :: [Point] -> (Point, Point)
findMinMax [] = error "List cannot be empty!"
findMinMax (p:ps) = fmm' (p,p) ps
    where   fmm' pp [] = pp
            fmm' ((mnX,mnY),(mxX,mxY)) ((x,y):ps)
              = fmm' ((min x mnX, min y mnY), (max x mxX, max y mxY)) ps

contains :: Eq a => a -> [a] -> Bool
contains _ [] = False
contains el (x:xs) 
  | el == x = True
  | otherwise = contains el xs

main = do
    input <- getContents
    let inputLines = lines input
    let [pointsLines,opLines] = splitAtFirst "" inputLines
    let points = map parsePoint pointsLines

    let ops = map parseOp opLines

    putStrLn "Part I:"
    print $ length $ applyFold (head ops) points

    let finalPoints = foldl (flip applyFold) points ops
    let ((mnX,mnY),(mxX,mxY)) = findMinMax finalPoints

    let result = unlines [[if contains (x,y) finalPoints then '#' else ' ' | x <- [mnX..mxX]] | y <- [mnY..mxY]]

    putStrLn "Part II:"
    putStrLn result


