import System.IO

solution :: [[Int]] -> Int
solution xs = foldl (\acc x -> if x then acc + 1 else acc) 0 values
        where values = map isSafe xs

isSafe :: [Int] -> Bool
isSafe xs = all (\x -> x > 0 && x < 4) diff || all (\x -> x < 0 && x > -4) diff
        where diff = getDiff xs

getDiff :: [Int] -> [Int]
getDiff (x:y:[]) = [x - y]
getDiff (x:y:xs) = [x - y] ++ getDiff (y:xs)

toInt :: [[Char]] -> [Int]
toInt xs = map (\x -> read x ::Int) xs

main :: IO()
main = do 
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        let lines' = lines contents
        let levels = map (toInt . words) lines'
        print $ solution levels