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

count :: Eq a => a -> [a] -> Int
count el xs = length $ filter (==el) xs

simulate :: Int -> [Int] -> [Int]
simulate _ [] = []
simulate 0 cnts = cnts
simulate day (c0:cnts) = do
    let (c6:rst) = drop 6 cnts
    simulate (day-1) $ take 6 cnts ++ [c0 + c6] ++ rst ++ [c0]

main = do
    line <- getLine
    let nums = map toInt $ splitOn ',' line

    let cnts = map (`count` nums) [0..8]

    putStrLn "Part I:"
    print $ sum $ simulate 80 cnts

    putStrLn "Part II:"
    print $ sum $ simulate 256 cnts




