import Data.List
import System.IO
solution :: [Int] -> [Int] -> Int
solution a b = foldl (\acc x -> (+) acc $ abs $ fst x - snd x) 0 $ zip left right
            where left = sort a
                  right = sort b
toInt :: [[Char]] -> [Int]
toInt xs = map (\x -> read x ::Int) xs
main :: IO()
main = do 
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        let lines' = lines contents
        let pairs = map (\line -> words line) lines'
        let a = map head pairs
        let b = map (head . tail) $ pairs
        print $ solution (toInt a) (toInt b)
        -- let a = map fst tuples