import           Control.Monad.Trans.State
import qualified Data.Map                  as Map
import Data.Maybe
import Data.Bifunctor

dropTail :: [a] -> [a]
dropTail []     = error "Empty list!"
dropTail [x]    = []
dropTail (x:xs) = x : dropTail xs

getOnIds :: [Int] -> [a] -> [a]
getOnIds ids arr = map (arr !!) ids

    {-

#############
#0123456789A#
###A#B#C#D###
  #.#.#.#.#
  #########

        -}

type SimState = (String,[String])

type MyStateMemo = State (Map.Map SimState (Maybe Int))


roomPos = [2,4,6,8]
typs = "ABCD"

typToCost :: Char -> Int
typToCost 'A' = 1
typToCost 'B' = 10
typToCost 'C' = 100
typToCost 'D' = 1000
typToCost t   = error $ "error unknown typ" ++ [t]

typToRoom :: Char -> Int
typToRoom 'A' = 0
typToRoom 'B' = 1
typToRoom 'C' = 2
typToRoom 'D' = 3
typToRoom t   = error $ "error unknown typ" ++ [t]

typToRoomPos :: Char -> Int
typToRoomPos 'A' = head roomPos
typToRoomPos 'B' = roomPos !! 1
typToRoomPos 'C' = roomPos !! 2
typToRoomPos 'D' = roomPos !! 3
typToRoomPos t   = error $ "error unknown typ" ++ [t]

moveToward :: Int -> Int -> Int
moveToward from to
  | from < to = from + 1
  | otherwise = from - 1

replaceAt :: a -> Int -> [a] -> [a]
replaceAt el i arr = take i arr ++ [el] ++ drop (i+1) arr

memoize :: SimState -> Maybe Int -> MyStateMemo (Maybe Int)
memoize st n = do
    mp <- get
    put $ Map.insert st n mp
    return n

simulate :: SimState -> Int -> MyStateMemo (Maybe Int)
simulate st@(corridor, rooms) roomCap = do
    mp <- get
    case mp Map.!? st of
      Just v -> return v
      Nothing -> do
        let allWays = giveAllWays st roomCap
        outcomes <- mapM (\(s,c) -> simulate s roomCap) allWays
        let outcomes' = map (first fromJust) $ filter (isJust . fst) $ zip outcomes $ map snd allWays
        if null outcomes' then memoize st Nothing
                          else memoize st $ Just $ minimum $ map (uncurry (+)) outcomes' 

giveAllWays :: SimState -> Int -> [(SimState,Int)]
giveAllWays st roomCap = do
        let leaving = concatMap (\i -> waysToLeave i st roomCap) [0..3] :: [(SimState, Int)]
        let entering = concatMap (\p -> waysToEnter p st roomCap) [0..10] :: [(SimState, Int)]
        leaving ++ entering

waysToLeave ri st@(corridor, rooms) roomCap =
   if null (rooms !! ri) || all (==(typs !! ri)) (rooms !! ri)
      then []
      else do
             let rp = roomPos !! ri
             let typ = head (rooms !! ri)
             let roomsWithoutI = map (\i -> if i == ri then drop 1 (rooms !! i) else rooms !! i) [0..3]
             map (\(nc,cost) -> ((nc,roomsWithoutI),typToCost typ * (cost + (roomCap + 1 - length (rooms !! ri))))) $
                 tryWalkLeft rp corridor typ rp  ++
                     tryWalkRight rp corridor typ rp
waysToEnter pos st@(corridor, rooms) roomCap = do
    let pod = corridor !! pos
    let targetRoom = rooms !! typToRoom pod
    if pod == '.' || pod == '_' || any (/=pod) targetRoom then []
                                else do
                                    let roomId = typToRoom pod
                                    let newRooms = map (\i -> if i == roomId then pod : rooms !! i else rooms !! i) [0..3]
                                    [
                                        ((replaceAt '.' pos corridor, newRooms),
                                            typToCost pod * ((roomCap - length (rooms !! roomId)) + abs (pos - typToRoomPos pod)))
                                            | tryEnterRoom pos corridor pod]
tryWalkLeft, tryWalkRight :: Int -> String -> Char -> Int -> [(String,Int)]
tryWalkLeft pos corridor pod stPos
  | pos < 0 = []
  | corridor !! pos == '.' = (replaceAt pod pos corridor, stPos - pos) : tryWalkLeft (pos-1) corridor pod stPos
  | corridor !! pos == '_' = tryWalkLeft (pos-1) corridor pod stPos
  | otherwise = []
tryWalkRight pos corridor pod stPos
  | pos >= length corridor = []
  | corridor !! pos == '.' = (replaceAt pod pos corridor, pos - stPos) : tryWalkRight (pos+1) corridor pod stPos
  | corridor !! pos == '_' = tryWalkRight (pos+1) corridor pod stPos
  | otherwise = []
tryEnterRoom pos corridor pod = do
    let targetPos = typToRoomPos pod
    let newPos = moveToward pos targetPos
    not (newPos < 0 || newPos >= length corridor)
        && ((newPos == targetPos)
            || ((corridor !! newPos == '.' || corridor !! newPos == '_')
                && tryEnterRoom newPos corridor pod))

endState = (".._._._._..",["AA","BB","CC","DD"]) :: SimState
endState2 = (".._._._._..",["AAAA","BBBB","CCCC","DDDD"]) :: SimState

main :: IO ()
main = do
    input <- getContents
    let startRooms = foldr (zipWith (:) . getOnIds [3,5,7,9]) ["","","",""]  $ dropTail $ drop 2 $ lines input
    let roomCap = length $ head startRooms
    let startState = (".._._._._..",startRooms)
    let startMemo = Map.fromList [(endState,Just 0)]

    putStrLn "Part I:"
    print $ fromJust $ evalState (simulate startState roomCap) startMemo 

    let startRooms2 = zipWith (\a b -> [head a] ++ b ++ [last a]) startRooms ["DD","CB","BA","AC"]
    let roomCap2 = length $ head startRooms2
    let startState2 = (".._._._._..",startRooms2)
    let startMemo2 = Map.fromList [(endState2,Just 0)]

    putStrLn "Part II:"
    print $ fromJust $ evalState (simulate startState2 roomCap2) startMemo2

