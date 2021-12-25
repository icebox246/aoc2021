import qualified Data.Map.Strict as Map
import Data.Maybe

type Grid = Map.Map (Int,Int) Char
type Bounds = (Int,Int)
type Point = (Int,Int)

roundToBounds :: Point -> Bounds -> Point
roundToBounds (x,y) (mx,my) = (x `mod` mx, y `mod` my)

applyStep :: Grid -> Bounds -> Grid
applyStep grid bnds = do
    let grid' = foldl (\mp ((x,y),t) -> if t == '>'
                                           then do
                                                let newPos = roundToBounds (x+1,y) bnds
                                                if isNothing (grid Map.!? newPos)
                                                    then Map.insert newPos t mp
                                                    else Map.insert (x,y) t mp
                                           else Map.insert (x,y) t mp
                                           ) Map.empty $ Map.toList grid
    let grid'' = foldl (\mp ((x,y),t) -> if t == 'v'
                                           then do
                                                let newPos = roundToBounds (x,y+1) bnds
                                                if isNothing (grid' Map.!? newPos)
                                                    then Map.insert newPos t mp
                                                    else Map.insert (x,y) t mp
                                           else Map.insert (x,y) t mp
                                           ) Map.empty $ Map.toList grid'
    grid''

findLastStep :: Grid -> Bounds -> Int
findLastStep grid bnds = find' grid 1
    where 
        find' grid n = do
            let grid' = applyStep grid bnds
            if grid' == grid then n
                             else find' grid' (n+1)


main :: IO ()
main = do
    input <- getContents
    let inputLines = lines input
    let height = length inputLines
    let width = length $ head inputLines
    let startMap = foldl (\mp p@(x,y) ->
            let t = inputLines !! y !! x
            in if t /= '.' then Map.insert p t mp else mp)
            Map.empty
            [(x,y) | y <- [0..height-1], x <- [0..width-1]]

    putStrLn "Part I:"
    print $ findLastStep startMap (width,height)


