import System.IO

solution xs = foldl (\acc (x, y) -> acc+y) 0 filtered
        where numbers = map words xs
              solutions = map (toInt . init . head) numbers
              products = map (toInts . tail)  numbers
              combs x = getAllCombinations $ [1..x]
              filtered = filter (\(x, y) -> any (==y) (map (getNum x) $ combs (length x - 1)) ) $ zip products solutions

getNum (x:[]) [] = x
getNum (x:y:xs) (c:cs) = if c == '+' then getNum (x+y:xs) cs else getNum (x*y:xs) cs

getAllCombinations :: [x] -> [[Char]]
getAllCombinations [] = [[]]
getAllCombinations (x:xs) = map (\c -> c ++ ['+']) (getAllCombinations xs) ++ map (\c -> c ++ ['*']) (getAllCombinations xs) 


toInts :: [[Char]] -> [Int]
toInts xs = map toInt xs

toInt :: [Char] -> Int
toInt x = (read x) :: Int

main :: IO()
main = do 
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        print $ solution $ lines contents 