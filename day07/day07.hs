toInt :: String -> Int
toInt s = read s :: Int


splitOn :: Eq a => a -> [a] -> [[a]]
splitOn el xs = splitOn' xs []
    where
        splitOn' [] buf = [buf]
        splitOn' (x:xs) buf
          | x == el = buf : splitOn' xs []
          | otherwise = splitOn' xs (buf ++ [x])

calcFuel :: Int -> [Int] -> Int
calcFuel tg poss = sum $ map (\p -> abs (p - tg)) poss

sumN :: Int -> Int
sumN n = n * (n+1) `div` 2

calcFuel2 :: Int -> [Int] -> Int
calcFuel2 tg poss = sum $ map (\p -> sumN $ abs (p - tg)) poss

main = do
    line <- getLine
    let poss = map toInt $ splitOn ',' line
    let minPos = minimum poss
    let maxPos = maximum poss

    putStrLn "Part I:"
    print $ minimum $ map (`calcFuel` poss) [minPos..maxPos]

    putStrLn "Part II:"
    print $ minimum $ map (`calcFuel2` poss) [minPos..maxPos]

