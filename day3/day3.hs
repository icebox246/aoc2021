cntOccs :: [String] -> Int -> Int
cntOccs [] pos = 0
cntOccs (s:ss) pos
  | s !! pos == '1' = 1 + cntOccs ss pos
  | otherwise = cntOccs ss pos

calcGamma :: [String] -> Int -> [Int]
calcGamma arr pos
  | pos < length (head arr) = (if cntOccs arr pos >= length arr `div` 2 then 1 else 0) : calcGamma arr (pos+1)
  | otherwise = []

binArrToInt :: [Int] -> Int
binArrToInt arr = foo arr 0
    where
        foo [] acc     = acc
        foo (x:xs) acc = foo xs (acc * 2 + x)

binStrToInt :: String -> Int
binStrToInt arr = binArrToInt $ map (\e -> if e == '1' then 1 else 0) arr

searchO2 :: [String] -> Int -> String
searchO2 [lst] _ = lst
searchO2 arr pos
  | pos < length (head arr) = do
    let cnt1 = cntOccs arr pos
    if cnt1 * 2 >= length arr then
                                 searchO2 (filter (\e -> (e !! pos) == '1') arr) (pos+1)
                                 else
                                 searchO2 (filter (\e -> (e !! pos) == '0') arr) (pos+1)
  | otherwise = error "Exceeded table!"

searchCO2 :: [String] -> Int -> String
searchCO2 [lst] _ = lst
searchCO2 arr pos
  | pos < length (head arr) = do
    let cnt1 = cntOccs arr pos
    if cnt1 * 2 < length arr then
                                 searchCO2 (filter (\e -> (e !! pos) == '1') arr) (pos+1)
                                 else
                                 searchCO2 (filter (\e -> (e !! pos) == '0') arr) (pos+1)
  | otherwise = error "Exceeded table!"

main = do
    input <- getContents
    let inputLines = lines input
    let len = length $ head inputLines
    let gammaArr = calcGamma inputLines 0
    let gamma = binArrToInt gammaArr
    let epsilonArr = map (1 -) gammaArr
    let epsilon = binArrToInt epsilonArr
    print gamma
    print epsilon
    putStrLn "Part I:"
    print $ gamma * epsilon
    let o2Rating = binStrToInt $ searchO2 inputLines 0
    print o2Rating
    let co2Rating = binStrToInt $ searchCO2 inputLines 0
    print co2Rating
    putStrLn "Part II:"
    print $ o2Rating * co2Rating
