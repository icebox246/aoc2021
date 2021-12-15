import           Data.List
import qualified Data.Map  as M
type Poly = (String, Char)

parsePoly :: String -> Poly
parsePoly s = let ws = words s in (head ws, head (ws !! 2))                                   Just c  -> apply' (c:s0:buf) (s1:ss)

countOcs :: Ord a => [a] -> M.Map a Int
countOcs [] = error "No elements to count!"
countOcs arr = let (x:xs) = sort arr in M.fromList (co' x 1 $ sort xs)
    where
        co' el n [] = [(el,n)]
        co' el n (x:xs)
            | x == el = co' el (n+1) xs
            | otherwise = (el,n) : co' x 1 xs

pairs :: [a] -> [[a]]
pairs (x0:x1:xs) = [x0,x1] : pairs (x1:xs)
pairs _          = []

applyPoly :: M.Map String Int -> M.Map Char Int ->  M.Map String Char -> (M.Map String Int,M.Map Char Int)
applyPoly state cstate mp = apply' (M.toList state) M.empty cstate
    where
        apply' :: [(String,Int)] -> M.Map String Int -> M.Map Char Int -> (M.Map String Int,M.Map Char Int)
        apply' [] buf cbuf = (buf,cbuf)
        apply' ((s@[s0,s1],c):xs) buf cbuf = case M.lookup s mp of
                                               Nothing -> apply' xs (M.insertWith (+) s c buf) cbuf
                                               Just ch -> apply' xs (M.insertWith (+) [s0,ch] c $ M.insertWith (+) [ch,s1] c buf) (M.insertWith (+) ch c cbuf)
        apply' _ _ _ = error "Unexpected!"

main = do
    input <- getContents
    let inputLines = lines input
    let startingState = head inputLines
    let polys = map parsePoly $ drop 2 inputLines
    let polyMap = M.fromList polys
    let startMap = countOcs $ pairs startingState

    let finalMap = foldl (\ (pairMap,charMap) _ -> applyPoly pairMap charMap polyMap) (startMap,countOcs startingState) [1..10]
    let finalLetters = snd finalMap
    let occsInFinal =  sortBy (\(_,a) (_,b) -> compare a b) $ M.toList finalLetters
    putStrLn "Part I:"
    print $ snd (last occsInFinal) - snd (head occsInFinal)

    let finalMap' = foldl (\ (pairMap,charMap) _ -> applyPoly pairMap charMap polyMap) (startMap,countOcs startingState) [1..40]
    let finalLetters' = snd finalMap'
    let occsInFinal' =  sortBy (\(_,a) (_,b) -> compare a b) $ M.toList finalLetters'
    putStrLn "Part II:"
    print $ snd (last occsInFinal') - snd (head occsInFinal')

