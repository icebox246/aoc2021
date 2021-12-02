toInt :: String -> Int
toInt s = read s :: Int

searchRaises :: [Int] -> Int
searchRaises [] = 0
searchRaises (x:xs) = searchRaises' x xs
    where
        searchRaises' :: Int -> [Int] -> Int
        searchRaises' _ [] = 0
        searchRaises' lst (x:xs)
            | x > lst = 1 + searchRaises' x xs
            | otherwise = searchRaises' x xs

sum3 :: [Int] -> [Int]
sum3 (x:y:z:rst) = (x+y+z) : sum3 (y:z:rst)
sum3 arr         = []

main = do
    inputLines <- getContents
    let nums = map toInt $ lines inputLines
    putStrLn "Part I:"
    print $ searchRaises nums
    putStrLn "Part II:"
    print $ searchRaises $ sum3 nums
