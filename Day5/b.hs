import System.IO
import qualified Data.Map as Map
import Data.Maybe
import Data.List
import Data.Char

solution xs = foldl (\acc x -> acc + toInt x) 0 $ map (midPoint []) unzipped-- head (map getArr filteredSequences)
        where graph = takeWhile (/="") $ lines xs
              sequences = drop 1 $ dropWhile (/="") $ lines xs
              graphMap = buildMap graph
              brokenSequences = map (splitOn ',') sequences
              checked = map (\x -> check [] x graphMap) brokenSequences
              filteredSequences = map snd $ filter (\x -> fst x) $ zip checked brokenSequences
              getArr xs = map (\x -> (fromMaybe [] $ Map.lookup x graphMap) `intersect` xs) xs
              lengthArr = map (map length) (map getArr filteredSequences)
              zipped = map (\x -> fst x `zip` snd x)(zip filteredSequences lengthArr)
              sorted = map (sortBy (\x y -> snd x `compare` snd y)) zipped
              unzipped = map (map fst) sorted
            --   sorted = map (sortBy (\x y -> length x `compare` length y)) (map getArr filteredSequences)

midPoint prev (x:xs) = if (length prev == length xs) then x else midPoint (x:prev) xs

check prev [] graphMap = False
check prev (x:xs) graphMap = bad || check (x:prev) xs graphMap
        where array = fromMaybe [] $ Map.lookup x graphMap
              bad = if length (prev `intersect` array) == 0 then False else True

splitString str x = [left, right]
        where left = takeWhile (/=str) x
              right = drop 1 $ dropWhile (/=str) x

splitOn str x = filter (not . any (==str)) . groupBy (\x y -> (x/=str) == (y/=str)) $ x

buildMap (x:[]) = Map.singleton left right
        where (left:right) = splitString '|' x

buildMap (x:xs) = Map.insert left ((head right):array) $ map
        where (left:right) = splitString '|' x
              map = buildMap xs
              array = fromMaybe [] $ Map.lookup left map

toInt x = (read x ::Int)        

main :: IO()
main = do 
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        print $ solution contents 