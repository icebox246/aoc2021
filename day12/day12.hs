import           Data.Char
import           Data.List

sList :: [String] -> String
sList [] = []
sList [a] = a
sList (a:as) = a ++ "," ++ sList as

isStrLower :: String -> Bool
isStrLower = all isLower

halveAt :: (a -> Bool) -> [a] -> ([a],[a])
halveAt fn = part' fn []
    where
        part' :: (a -> Bool) -> [a] -> [a] -> ([a],[a])
        part' _ lft [] = (lft,[])
        part' fn lft (r:rs)
            | fn r = (lft,rs)
            | otherwise = part' fn (lft ++ [r]) rs

mirrorPairs :: [(a,a)] -> [(a,a)]
mirrorPairs [] = []
mirrorPairs ((x,y):rst) =
    (x,y):(y,x):mirrorPairs rst


groupOnFst :: Eq a => [(a,b)] -> [(a,[b])]
groupOnFst arr = do
    let gp = groupBy (\p q-> fst p == fst q) arr
    map (\g -> do
            let (q,b) = head g
            foldl (\(q,bs) (_,b) -> (q,b:bs)) (q,[b]) (drop 1 g)
        ) gp

getBranches :: Eq a => a -> [(a,[a])] -> [a]
getBranches _ [] = []
getBranches el ((p,cs):bs)
  | p == el = cs
  | otherwise = getBranches el bs

paths ::  String -> String -> [(String,[String])] -> [[String]]
paths st tg graph = trav' st []
    where trav' cnt path
            | cnt == tg = [reverse $ cnt:path]
            | isStrLower cnt && elem cnt path = []
            | otherwise = do
                let children = getBranches cnt graph
                concat [ trav' c (cnt:path) | c <- children ]

paths2 ::  String -> String -> [(String,[String])] -> [[String]]
paths2 st tg graph = trav' st [] False
    where trav' cnt path used2
            | cnt == tg = [reverse $ cnt:path]
            | otherwise = do
                let children = getBranches cnt graph
                concat [ trav' c (cnt:path) (used2 || (isStrLower c && elem c path)) |
                    c <- children, not (isStrLower c) || (not used2 || notElem c path), c /= st ]

main = do
    input <- getContents
    let graph = groupOnFst $ sort $ mirrorPairs $ map (halveAt (=='-')) $ lines input

    let pathsSE = paths "start" "end" graph

    putStrLn "Part I:"
    print $ length pathsSE

    let pathsSE2 = paths2 "start" "end" graph

    putStrLn "Part II:"
    print $ length pathsSE2
