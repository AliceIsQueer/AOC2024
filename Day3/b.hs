import System.IO
import Data.List
import Data.Char

-- This code was written at 7am in the morning, having not slept through the night, please have mercy

solution :: [Char] -> Int
solution xs = foldl (\acc x -> if (isSafe (snd x) dos donts) then acc + fst x else acc) 0 mulsNdIndices
        where splits = findString xs "mul(" 0
              dos = 0:findString xs "do()" 0
              donts = findString xs "don't()" 0
              chunks = map (\x -> take 8 $ drop x xs) splits
              validChunks = filter (isValid . snd) (zip splits chunks)
              muls = map getNums $ map snd validChunks
              mulsNdIndices = zip muls $ map fst validChunks

isSafe x dos donts = if length lastDont == 0 || last lastDo > last lastDont then True else False
        where lastDo = fst $ span (<x) dos
              lastDont = fst $ span (<x) donts

getNums xs = left * right
        where left = read (head $ commaSplit xs) :: Int
              right = read (secondNums $ second $ commaSplit xs) :: Int

isValid xs = length (commaSplit xs) > 1 && all isNumber (head $ commaSplit xs) && all isNumber (secondNums $ second $ commaSplit xs)
              
commaSplit xs = groupBy (\x y -> (x/=',') && (y/=',')) xs

second x = head $ drop 2 x

secondNums x = takeWhile (/=')') x

findString "" str ind = []
findString xs str ind = if take (length str) xs == str then (ind+(length str)):findString (drop 1 xs) str (ind+1) else findString (drop 1 xs) str (ind+1)


toInt :: [[Char]] -> [Int]
toInt xs = map (\x -> read x ::Int) xs

main :: IO()
main = do 
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        print $ solution contents 