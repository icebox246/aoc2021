genCommand :: String -> (String, Int)
genCommand s = do
    let tokens = words s
    (head tokens, read (tokens !! 1) :: Int)

runCommands :: (Int,Int) -> [(String, Int)] -> (Int,Int)
runCommands pos [] = pos
runCommands pos (comm:rst) = runCommands (handleComm pos comm) rst
    where handleComm (px,py) (dir,dist)
            | dir == "forward" = (px + dist,py)
            | dir == "up" = (px,py-dist)
            | dir == "down" = (px,py+dist)
            | otherwise = error "Wrong command!"

runCommands2 :: (Int,Int,Int) -> [(String, Int)] -> (Int,Int,Int)
runCommands2 pos [] = pos
runCommands2 pos (comm:rst) = runCommands2 (handleComm pos comm) rst
    where handleComm (px,py,aim) (dir,dist)
            | dir == "forward" = (px + dist,py + dist * aim,aim)
            | dir == "up" = (px,py,aim-dist)
            | dir == "down" = (px,py,aim+dist)
            | otherwise = error "Wrong command!"

multPair :: (Int, Int) -> Int
multPair (a,b) = a * b

tripleToPair :: (a,a,a) -> (a,a)
tripleToPair (a,b,c) = (a,b)

main = do
    inputLines <- getContents
    let commands = map genCommand $ lines inputLines
    putStrLn "Part I:"
    print $ multPair $ runCommands (0,0) commands
    putStrLn "Part II:"
    print $ multPair $ tripleToPair $ runCommands2 (0,0,0) commands

