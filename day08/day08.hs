type Entry = ([String], [String])

makeEntry :: String -> Entry
makeEntry s = let ws = words s in (take 10 ws, drop 11 ws)

contains :: Eq a => a -> [a] -> Bool
contains _ [] = False
contains el (x:xs)
  | x == el = True
  | otherwise = contains el xs

count :: (a -> Bool) -> [a] -> Int
count _ [] = 0
count fn (x:xs)
  | fn x = 1 + count fn xs
  | otherwise = count fn xs

main = do
    input <- getContents
    let entries = map makeEntry $ lines input

    putStrLn "Part I:"
    print $ sum $ map (count (==True) . map ((\x -> contains x [2,4,3,7]) . length). snd) entries
