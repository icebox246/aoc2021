import           Data.List


toInt :: String -> Int
toInt n = read n :: Int

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn el arr = splitOn' el arr []
    where
        splitOn' :: Eq a => a -> [a] -> [a] -> [[a]]
        splitOn' _ [] buf = [buf]
        splitOn' el (a:as) buf
            | a == el && buf /= [] = buf: splitOn' el as []
            | a == el && null buf = splitOn' el as []
            | otherwise = splitOn' el as (buf ++ [a])


eraseAtIds :: [Int] -> [a] -> [a]
eraseAtIds is as = eraseAtIds' (sort is) as 0
    where eraseAtIds' (i:is) (a:as) pos
            | i == pos = eraseAtIds' is as (pos+1)
            | otherwise = a : eraseAtIds' (i:is) as (pos+1)
          eraseAtIds' [] [] _ = []
          eraseAtIds' [] as _ = as
          eraseAtIds' _ [] _ = error "Index out of bounds!"


type Bingo = [Int]

checkGame :: [Int] -> [Bingo] -> Int
checkGame [] _ = error "Game unwinnable!"
checkGame (n:ns) bingos = do
    let bingosAfterApply = applyNumber n bingos
    let winningIds = findWin bingosAfterApply 0 :: [Int]
    let winningIdx = if not (null winningIds) then head winningIds else (-1)
    if winningIdx < 0 then
                      checkGame ns bingosAfterApply
                      else
                      n * sum (filter (\x -> x /= (-1)) (bingosAfterApply !! winningIdx))

findLastWin :: [Int] -> [Bingo] -> Int
findLastWin [] bingos = error $ "Could not find last game!" ++ show bingos
findLastWin (n:ns) bingos = do
    let bingosAfterApply = applyNumber n bingos
    let winningIds = findWin bingosAfterApply 0
    let bingosFiltered = eraseAtIds winningIds bingosAfterApply
    if null bingosFiltered then n * sum (filter (\x -> x /= (-1)) (head bingosAfterApply))
                           else findLastWin ns bingosFiltered

applyNumber :: Int -> [Bingo] -> [Bingo]
applyNumber _ [] = []
applyNumber n (b:bs) = map (\x -> if x == n then (-1) else x) b : applyNumber n bs

findWin :: [Bingo] -> Int -> [Int]
findWin [] _ = []
findWin (b:bs) id
  | findRow b || findColumn b 0 = id : findWin bs (id+1)
  | otherwise = findWin bs (id+1)
    where
        findRow (b1:b2:b3:b4:b5:bs)
            | all (==(-1)) [b1,b2,b3,b4,b5] = True
            | otherwise = findRow bs
        findRow _ = False
        findColumn bs id
          | id >= 5 = False
          | all (==(-1)) (getColumn bs id 5) = True
          | otherwise = findColumn bs (id+1)

getColumn :: [a] -> Int -> Int -> [a]
getColumn bs x width = map ((bs !!) . (\y -> y * width + x)) [0..((length bs `div` width) -1)]


main = do
    input <- getContents
    let inputLines = lines input
    let (numbersLine:_:bingoLines) = inputLines
    let numbers = map toInt $ splitOn ',' numbersLine
    let bingos = map (map toInt . words . unwords) (splitOn "" bingoLines) :: [Bingo]
    putStrLn "Part I:"
    print $ checkGame numbers bingos
    putStrLn "Part II:"
    print $ findLastWin numbers bingos

