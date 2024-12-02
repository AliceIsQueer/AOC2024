import System.IO

solution :: [[Int]] -> Int
solution xs = foldl (\acc x -> if x then acc + 1 else acc) 0 values
        where values = map (\x -> isSafe x || (areSafe $ cull x []) || isSafe (init x) || isSafe (tail x)) diffs
              diffs = map getDiff xs

cull :: [Int] -> [Int] -> [[Int]]
cull (x:y:[]) prev = [prev ++ [x+y]]
cull (x:y:xs) prev = (prev ++ (x+y:xs)) : (cull (y:xs) (prev ++ [x]))

areSafe :: [[Int]] -> Bool
areSafe xs = any (== True) $ map isSafe xs

isSafe :: [Int] -> Bool
isSafe xs = all (\x -> x > 0 && x < 4) xs || all (\x -> x < 0 && x > -4) xs

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