charToInt :: Char -> Int
charToInt c = read [c] :: Int

idxToCoord :: Int -> (Int,Int)
idxToCoord n = (n `mod` 10, n `div` 10)

getVal :: (Int,Int) -> [Int] -> Int
getVal (x,y) grid
  | x < 0 || x > 9 || y < 0 || y > 9 = -1
  | otherwise = grid !! (x + y * 10)

countZeros :: [Int] -> Int
countZeros [] = 0
countZeros (x:xs)
  | x == 0 = 1 + countZeros xs
  | otherwise = countZeros xs

simulateStep :: [Int] -> [Int]
simulateStep grid = sim' $ map (+1) grid
    where
        sim' :: [Int] -> [Int]
        sim' grid
          | any (>9) grid = sim' $ map (`simCell'` grid) [0..99]
          | otherwise = grid
        simCell' :: Int -> [Int] -> Int
        simCell' idx prev = do
            let p@(x,y) = idxToCoord idx
            let pVal = getVal p prev
            if pVal > 0 && pVal <= 9 then
                pVal + sum [if getVal (x',y') prev > 9 then 1 else 0 | x' <- [x-1..x+1], y' <- [y-1..y+1], x' /= x || y' /= y ]
            else
                0

simulateSteps :: Int -> [Int] -> ([Int],Int)
simulateSteps 0 grid = (grid, countZeros grid)
simulateSteps n grid = do
    let (grid', zeros) = simulateSteps (n-1) $ simulateStep grid
    (grid', zeros + countZeros grid)

findFirstSync :: Int -> [Int] -> ([Int],Int)
findFirstSync n grid 
  | all (==0) grid = (grid,n)
  | otherwise = findFirstSync (n+1) $ simulateStep grid 

printState :: [Int] -> IO ()
printState xs = foldl1 (>>) $ printState' xs
    where
        printState' :: [Int] -> [IO ()]
        printState' [] = []
        printState' xs =
            putStrLn (foldl (\acc x -> acc ++ show x) "" $ take 10 xs) : printState' (drop 10 xs)

main = do
    input <- getContents
    let energyLevels = map charToInt $ foldl1 (++) $ lines input

    let (endState, flashes) = simulateSteps 100 energyLevels
    
    putStrLn "Part I:"
    print flashes

    let (endState2, firstSync) = findFirstSync 0 energyLevels

    putStrLn "Part II:"
    print firstSync

