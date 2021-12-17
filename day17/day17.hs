import Data.Char
import Data.Maybe

strToInt :: String -> Int
strToInt s = read s :: Int

isDigitMinus :: Char -> Bool
isDigitMinus c = isDigit c || c == '-'

type Point = (Int,Int)
type Vel = (Int,Int)
type Rect = (Point,Point)

parseInputLine :: String -> Rect
parseInputLine line = do
    let rst = dropWhile (not . isDigit) line
    let minX = strToInt$  takeWhile isDigitMinus rst
    let rst' = drop 2 $ dropWhile isDigitMinus rst
    let maxX = strToInt $ takeWhile isDigitMinus rst'
    let rst'' = drop 4 $ dropWhile isDigitMinus rst'
    let minY = strToInt $ takeWhile isDigitMinus rst''
    let rst''' = drop 2 $ dropWhile isDigitMinus rst''
    let maxY = strToInt $ takeWhile isDigitMinus rst'''
    ((minX,minY),(maxX,maxY))

inRange :: Int -> (Int,Int) -> Bool
inRange x (a,b)
  | a > b = inRange x (b,a)
  | x >= a && x <= b = True
  | otherwise = False

rectVsPoint :: Point -> Rect -> Bool
rectVsPoint (px,py) ((r1x,r1y),(r2x,r2y)) =
    inRange px (r1x,r2x) && inRange py (r1y,r2y)

moveToward0 :: Int -> Int
moveToward0 n
  | n < 0 = n + 1
  | n > 0 = n - 1
  | otherwise = 0

simulatePath :: Point -> Vel -> Rect -> Maybe Int
simulatePath = sim' 0
    where
        sim' :: Int -> Point -> Vel -> Rect -> Maybe Int
        sim' mh pos@(x,y) vel@(vx,vy) target@((r1x,r1y),(r2x,r2y))
            | rectVsPoint pos target = Just mh
            | y < r1y = Nothing
            | otherwise = sim' (max mh (y + vy)) (x + vx, y + vy) (moveToward0 vx, vy - 1) target


main = do
    line <- getLine
    let targetRect = parseInputLine line

    putStrLn "Part I:"
    print $ maximum $ catMaybes [simulatePath (0, 0) (vx, vy) targetRect  | vx <- [-100..100], vy <- [0..100]]

    let velocities = map fst $
                        filter (isJust . snd) 
                            [((vx, vy), simulatePath (0, 0) (vx, vy) targetRect) | vx <- [-320..320], vy <- [-80..250]]
    putStrLn "Part II:"
    print $ length velocities




