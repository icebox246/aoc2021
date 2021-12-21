import           Control.Monad.State
import           Control.Monad.Writer
import           Data.List
import qualified Data.Map as Map
strToInt :: String -> Int
strToInt s = read s :: Int

determDice = [ x `mod` 100 + 1 | x <- [0..] ]

roundToBoard :: Int -> Int
roundToBoard n = (n - 1) `mod` 10 + 1

simGame :: Int -> Int -> Int -> Int -> Int -> [Int] -> (Int,Int,Int,Int)
simGame p1 p2 s1 s2 mvnum ds
    | s1 >= 1000 = (1,mvnum - 1, s1, s2)
    | s2 >= 1000 = (2,mvnum - 1, s2, s1)
    | odd mvnum  =
        let np1 = roundToBoard (p1 + sum (take 3 ds)) in simGame np1 p2 (s1 + np1) s2 (mvnum + 1) (drop 3 ds)
    | even mvnum =
        let np2 = roundToBoard (p2 + sum (take 3 ds)) in simGame p1 np2 s1 (s2 + np2) (mvnum + 1) (drop 3 ds)
simGame _ _ _ _ _ _ = error "Wrong state"

dizarRolls :: [Int]
dizarRolls = [ sum [r1,r2,r3] | r1 <-[1..3], r2 <-[1..3], r3 <-[1..3] ]

sumPair :: (Int, Int) -> (Int, Int) -> (Int, Int)
sumPair (a1,a2) (b1,b2) = (a1 + b1, a2 + b2)

type GameState = (Int , Int , Int , Int , Bool)

insertS :: GameState -> (Int,Int) -> State (Map.Map GameState (Int,Int)) (Int,Int)
insertS gs v = do
    mp <- get
    put $ Map.insert gs v mp
    return v

simGame2 :: GameState -> State (Map.Map GameState (Int,Int)) (Int,Int)
simGame2 gs@(p1,p2,s1,s2,turn1) = do
    mp <- get
    case mp Map.!? gs of
      Just v -> return v
      Nothing -> chk gs
        where chk gs@(p1,p2,s1,s2,turn1)
                | s1 >= 21 = do
                    insertS gs (1,0)
                | s2 >= 21 = do
                    insertS gs (0,1)
                | turn1 = do
                    outcome <- mapM (\d ->
                        let np = roundToBoard (p1 + d) in simGame2 (np,p2,s1+np,s2,not turn1)) dizarRolls
                    let value = foldl sumPair (0,0) outcome
                    insertS gs value
                    return value
                | not turn1 = do
                    outcome <- mapM (\d ->
                        let np = roundToBoard (p2 + d) in simGame2 (p1,np,s1,s2+np,not turn1)) dizarRolls
                    let value = foldl sumPair (0,0) outcome
                    insertS gs value
              chk (_,_,_,_,False) = error "Was ist los?"


main :: IO ()
main = do
    player1Line <- getLine
    player2Line <- getLine

    let pos1 = strToInt . last $ words player1Line
    let pos2 = strToInt . last $ words player2Line

    let winState1@(_,moves1,_,losingScore1) = simGame pos1 pos2 0 0 1 determDice

    putStrLn "Part I:"
    print $ moves1 * 3 * losingScore1

    putStrLn "Part II:"
    print $ uncurry max $ evalState (simGame2 (pos1,pos2,0,0,True)) Map.empty
