import System.IO
import Data.List

solution :: [Char] -> Integer
solution xs = getDiagonal squares + getCount rows + getCount columns
        where all4x4s = map (map $ get4rows) (get4rows $ lines xs)
              combine xs = map concat xs
              fourBy4s = map (combine . transpose ) all4x4s 
              rows = concat $ map get4rows $ lines xs
              columns = concat $ map get4rows $ transpose $ lines xs
              squares = concat fourBy4s 
              check str = if str == "XMAS" || str == "SAMX" then 1 else 0

getDiagonal :: [[Char]] -> Integer
getDiagonal xs = foldl (\acc x -> acc + (check . rightDiag $ x) + (check . leftDiag $ x)) 0 xs
        where check str = if str == "XMAS" || str == "SAMX" then 1 else 0

rightDiag :: [Char] -> [Char]
rightDiag xs = if length newxs < 3 then "" else head newxs : rightDiag (newxs)
        where newxs = drop 3 xs

leftDiag :: [Char] -> [Char]
leftDiag xs = if length xs == 0 then "" else head xs : leftDiag (drop 5 xs)

get4rows :: [a] -> [[a]]
get4rows (a:b:c:d:[]) = [[a,b,c,d]]
get4rows (x:xs) = (take 4 $ x:xs) : get4rows xs

getCount xs = foldl(\acc x -> acc + (check x)) 0 xs
        where check str = if str == "XMAS" || str == "SAMX" then 1 else 0 

main :: IO()
main = do 
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        print $ solution contents 