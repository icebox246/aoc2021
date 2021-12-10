import           Data.Maybe
import           Data.List

evalWrong :: Char -> Int
evalWrong ')' = 3
evalWrong ']' = 57
evalWrong '}' = 1197
evalWrong '>' = 25137
evalWrong _   = 0

evalMissing :: Char -> Int
evalMissing ')' = 1
evalMissing ']' = 2
evalMissing '}' = 3
evalMissing '>' = 4
evalMissing _   = 0

calcScoreMissing :: String -> Int
calcScoreMissing = foldl (\b c -> b * 5 + evalMissing c) 0

closer :: Char -> Char
closer '(' = ')'
closer '[' = ']'
closer '{' = '}'
closer '<' = '>'
closer _   = ' '

isCloser :: Char -> Bool
isCloser c = c `elem` ")]}>"


findWrong :: String -> Maybe Char
findWrong [] = Nothing
findWrong (s:ss) = findWrong' ss [s]
    where
        findWrong' :: String -> String -> Maybe Char
        findWrong' [] _ = Nothing
        findWrong' (s:ss) []
          | isCloser s = Just s
          | otherwise = findWrong' ss [s]
        findWrong' (s:ss) oss@(o:os)
          | isCloser s = if s == closer o then findWrong' ss os else Just s
          | otherwise = findWrong' ss (s:oss)

findMissing :: String -> String
findMissing [] = []
findMissing (s:ss) = findMissing' ss [s]
    where
        findMissing' :: String -> String -> String
        findMissing' [] oss = map closer oss
        findMissing' (s:ss) [] = findMissing' ss [s]
        findMissing' (s:ss) oss@(o:os)
          | isCloser s = findMissing' ss os
          | otherwise = findMissing' ss (s:oss)

main = do
    input <- getContents
    let inputLines = lines input
    putStrLn "Part I:"
    print $ sum $ map evalWrong $ mapMaybe findWrong inputLines

    let incompleteLines = filter (isNothing . findWrong) inputLines
    let missingScores = sort $ map (calcScoreMissing . findMissing) incompleteLines
    putStrLn "Part II:"
    print $ missingScores !! (length missingScores `div` 2)



