import System.IO
import Data.List
import Data.Char

-- This code was written at 7am in the morning, having not slept through the night, please have mercy

solution :: [Char] -> Int
solution xs = foldl (\acc x -> acc + x) 0 muls
        where splits = findMul xs 0
              chunks = map (\x -> take 8 $ drop x xs) splits
              validChunks = filter isValid chunks
              muls = map getNums validChunks

getNums xs = left * right
        where left = read (head $ commaSplit xs) :: Int
              right = read (secondNums $ second $ commaSplit xs) :: Int

isValid xs = length (commaSplit xs) > 1 && all isNumber (head $ commaSplit xs) && all isNumber (secondNums $ second $ commaSplit xs)
              
commaSplit xs = groupBy (\x y -> (x/=',') && (y/=',')) xs

second x = head $ drop 2 x

secondNums x = takeWhile (/=')') x

findMul "" ind = []
findMul xs ind = if take 4 xs == "mul(" then (ind+4):findMul (drop 1 xs) (ind+1) else findMul (drop 1 xs) (ind+1)

toInt :: [[Char]] -> [Int]
toInt xs = map (\x -> read x ::Int) xs

main :: IO()
main = do 
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        print $ solution contents 