import           Data.Bits
import           Data.Char

to4BitStr :: Int -> String
to4BitStr n = reverse $ map (\b -> if shift n (-b) .&. 1 == 1 then '1' else '0') [0..3]

hexChrToBin :: Char -> String
hexChrToBin c
  | isDigit c = to4BitStr $ ord c - ord '0'
  | otherwise = to4BitStr $ ord c - ord 'A' + 10

hexStrToBin :: String -> String
hexStrToBin = concatMap hexChrToBin

binStrToInt :: String -> Int
binStrToInt = bin' 0
    where
        bin' buf []     = buf
        bin' buf (x:xs) = bin' (buf * 2 + if x == '1' then 1 else 0) xs

data Packet = Lit Int Int
            | Opr Int Int [Packet]
            deriving (Show,Eq)

parsePacket :: String -> (Packet,String)
parsePacket s = do
    let (vers, s') = splitAt 3 s
    let (typs, s'') = splitAt 3 s'

    let ver = binStrToInt vers
    let typ = binStrToInt typs

    case typ of
      4 -> let (v,rst) = parseLitBody s'' in (Lit ver v, rst)
      t -> case head s'' of
             '0' -> let sz = binStrToInt (take 15 $ drop 1 s'') in
                        let (pcks,rst) = parsePackets (take sz (drop 16 s'')) in
                            (Opr ver typ pcks, rst ++ drop (16 + sz) s'')
             _ ->   let cnt = binStrToInt (take 11 $ drop 1 s'') in
                        let (pcks,rst) = parsePacketsCnt cnt (drop 12 s'') in
                            (Opr ver typ pcks, rst)

parsePackets :: String -> ([Packet],String)
parsePackets "" = ([],"")
parsePackets str = do
    let (pck,str') = parsePacket str
    let (pcks, str'') = parsePackets str' in (pck:pcks, str'')

parsePacketsCnt :: Int -> String -> ([Packet],String)
parsePacketsCnt 0 str = ([], str)
parsePacketsCnt n str
  | length str <= 6 = ([],str)
  | otherwise =
      if length str <= 7 then error "shite"
                  else do
                     let (pck,str') = parsePacket str
                     let (pcks, str'') = parsePacketsCnt (n-1) str' in (pck:pcks,str'')


parseLitBody :: String -> (Int,String)
parseLitBody [] = error "Tried to parse empty body of literate value"
parseLitBody str = let (val,len) = parseLitBody' 0 5 str in (val, drop len str)
    where
        parseLitBody' _ _ [] = error "Literate value didn't end with 0 beginning part"
        parseLitBody' buf len str@(s:ss)
          | s == '1' = parseLitBody' (buf * 2^4 + binStrToInt (take 4 ss)) (len+5) (drop 4 ss)
          | otherwise = (buf * 2 ^ 4 + binStrToInt (take 4 ss), len)

sumOfVer :: Packet -> Int
sumOfVer (Lit v _) = v
sumOfVer (Opr v _ pcks) = v + sum (map sumOfVer pcks)

evalPacket :: Packet -> Int
evalPacket (Lit _ val) = val
evalPacket (Opr _ typ pcks) =
    let ress = map evalPacket pcks in
        case typ of
          0 -> sum ress
          1 -> product ress
          2 -> minimum ress
          3 -> maximum ress
          5 -> if head ress > ress !! 1 then 1 else 0
          6 -> if head ress < ress !! 1 then 1 else 0
          7 -> if head ress == ress !! 1 then 1 else 0
          _ -> error "unknown operation"


main = do
    line <- getLine
    let binStr = hexStrToBin line
    let packets = fst $ parsePacket binStr

    putStrLn "Part I:"
    print $ sumOfVer packets
    putStrLn "Part II:"
    print $ evalPacket packets
