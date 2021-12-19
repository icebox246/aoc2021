import Data.Char
import GHC.TopHandler
import Control.Monad.IO.Class
type SailNum = [Sail]
data Sail = LBrack | RBrack | Val Int
    deriving (Show,Eq)

parseSailNum :: String -> SailNum
parseSailNum [] = []
parseSailNum str
  | head str == '[' = LBrack : parseSailNum (drop 1 str)
  | head str == ']' = RBrack : parseSailNum (drop 1 str)
  | head str == ',' = parseSailNum (drop 1 str)
  | otherwise = do
      let num = read (takeWhile isDigit str) :: Int
      let rst = dropWhile isDigit str
      Val num : parseSailNum rst

deVal :: Sail -> Int
deVal (Val n) = n
deVal t = error $ "Wrong sail token!: " ++ show t

isVal :: Sail -> Bool
isVal (Val _) = True
isVal _ = False

addVal :: Sail -> Int -> Sail
addVal (Val v) a = Val (v+a)
addVal _ _ = error "Wrong sail token!"

addToFirstOnLeft :: Int -> Int -> SailNum -> SailNum
addToFirstOnLeft val pos num
  | isVal (num !! pos) = take pos num ++ [addVal (num !! pos) val] ++ drop (pos+1) num
  | pos > 0 = addToFirstOnLeft val (pos-1) num
  | otherwise = num

addToFirstOnRight :: Int -> Int -> SailNum -> SailNum
addToFirstOnRight val pos num
  | isVal (num !! pos) = take pos num ++ [addVal (num !! pos) val] ++ drop (pos+1) num
  | pos < length num - 1 = addToFirstOnRight val (pos+1) num
  | otherwise = num

explodeSailNum :: SailNum -> SailNum
explodeSailNum num = red' 0 0 num
    where
        red' pos depth num
          | pos >= length num = num
          | depth >= 4 && num !! pos == LBrack && num !! (pos+3) == RBrack = do -- explode
              let f = deVal $ num !! (pos+1)
              let s = deVal $ num !! (pos+2)
              let num' = take pos num ++ [Val 0] ++ drop (pos+4) num
              let num'' = addToFirstOnLeft f (pos-1) num'
              let num''' = addToFirstOnRight s (pos+1) num''
              num'''
          | num !! pos == LBrack = red' (pos+1) (depth+1) num
          | num !! pos == RBrack = red' (pos+1) (depth-1) num
          | otherwise = red' (pos+1) depth num

splitSailNum :: SailNum -> SailNum
splitSailNum num = red' 0 0 num
    where
        red' pos depth num
          | pos >= length num = num
          | isVal (num !! pos) && deVal (num !! pos) >= 10 = do -- split
              let oldVal = deVal (num !! pos)
              let num' = take pos num
                            ++ [LBrack,Val (oldVal `div` 2), Val (oldVal `div` 2 + oldVal `mod` 2 ),RBrack]
                            ++ drop (pos+1) num
              num'
          | num !! pos == LBrack = red' (pos+1) (depth+1) num
          | num !! pos == RBrack = red' (pos+1) (depth-1) num
          | otherwise = red' (pos+1) depth num

reduceSailNum' :: SailNum -> SailNum
reduceSailNum' num = red' 0 0 num
    where
        red' pos depth num
          | pos >= length num = num
          | depth >= 4 && num !! pos == LBrack && num !! (pos+3) == RBrack = do -- explode
              let f = deVal $ num !! (pos+1)
              let s = deVal $ num !! (pos+2)
              let num' = take pos num ++ [Val 0] ++ drop (pos+4) num
              let num'' = addToFirstOnLeft f (pos-1) num'
              let num''' = addToFirstOnRight s (pos+1) num''
              num'''
          | isVal (num !! pos) && deVal (num !! pos) >= 10 = do -- split
              let oldVal = deVal (num !! pos)
              let num' = take pos num
                            ++ [LBrack,Val (oldVal `div` 2), Val (oldVal `div` 2 + oldVal `mod` 2 ),RBrack]
                            ++ drop (pos+1) num
              num'
          | num !! pos == LBrack = red' (pos+1) (depth+1) num
          | num !! pos == RBrack = red' (pos+1) (depth-1) num
          | otherwise = red' (pos+1) depth num


reduceSailNum :: SailNum -> SailNum
reduceSailNum num = do
    let exploded = explodeSailNum num
    if num == exploded
       then do
           let splitted = splitSailNum num
           if splitted == num
              then num
              else reduceSailNum splitted
       else reduceSailNum exploded

sailToString :: SailNum -> String
sailToString (LBrack:ns) = "[" ++ sailToString ns
sailToString (RBrack:ns) = "]" ++ (if not (null ns) && (isVal (head ns) || head ns == LBrack) then "," else "") ++ sailToString ns
sailToString ((Val v):ns) = show v ++ (if not (null ns) && (isVal (head ns) || head ns == LBrack) then "," else "") ++ sailToString ns
sailToString [] = []

addSailNums :: SailNum -> SailNum -> SailNum
addSailNums num1 num2 = do
    let added = [LBrack] ++ num1 ++ num2 ++ [RBrack]
    reduceSailNum added

sailMag :: SailNum -> Int
sailMag = fst . mag' 
    where 
        mag' :: SailNum -> (Int, SailNum)
        mag' [] = (0,[])
        mag' ((Val v):ns) = (v,ns)
        mag' (LBrack:ns) = do
            let (v1,ns') = mag' ns
            let (v2,ns'') = mag' ns'
            (v1 * 3 + v2 * 2, ns'')
        mag' (RBrack:ns) = mag' ns


main = do
    input <- getContents
    let sailNums = map parseSailNum $ lines input

    let totalSum = foldl1 addSailNums sailNums
    putStrLn $ sailToString totalSum
    let totalMag = sailMag totalSum

    putStrLn "Part I:"
    print totalMag

    let sailNumCnt = length sailNums
    let maxMag = maximum [sailMag $ addSailNums (sailNums !! i) (sailNums !! j) |
                        i <- [0..sailNumCnt - 1],
                        j <- [0..sailNumCnt - 1],
                        i/=j ]

    putStrLn "Part II:"
    print maxMag

