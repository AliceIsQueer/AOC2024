import System.IO
import Data.List

solution :: [Char] -> Int
solution xs = getDiagonal squares
        where all3x3s = map (map $ get3rows) (get3rows $ lines xs)
              combine xs = map concat xs
              threeBy3s = map (combine . transpose ) all3x3s
              squares = concat threeBy3s

getDiagonal :: [[Char]] -> Int
getDiagonal xs = foldl (\acc x -> if (check $ leftDiag x) && (check $ rightDiag x) then acc + 1 else acc) 0 xs
        where check str = str == "MAS" || str == "SAM"


rightDiag :: [Char] -> [Char]
rightDiag xs = if length newxs < 2 then "" else head newxs : rightDiag (newxs)
        where newxs = drop 2 xs

leftDiag :: [Char] -> [Char]
leftDiag xs = if length xs == 0 then "" else head xs : leftDiag (drop 4 xs)

get3rows :: [a] -> [[a]]
get3rows (a:b:c:[]) = [[a,b,c]]
get3rows (x:xs) = (take 3 $ x:xs) : get3rows xs


main :: IO()
main = do 
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        print $ solution contents 