import System.IO
import Data.List

solution xs = length $ nub $ concat $ map (takeWhile (\(x, y) -> x >= 0 && y >= 0 && x < xLength && y < yLength)) flatFuck
        where symbols = (nub $ unlines xs) \\ ".\n"
              places = map (\s -> findSymbols xs [] s 0 0) symbols
              combinations = map (\poles -> [(x, y) | x <- poles, y <- poles, x/=y]) places
              xLength = (length . head) xs
              yLength = length xs 
              antiNodes = map (map (\(one, two) -> getAntinodes one two)) combinations
              flatFuck = concat $ map concat antiNodes

getAntinodes one two = [[(yOne + n*yDiff, xOne + n*xDiff) | n<-[0..]], [(yTwo - n*yDiff, xTwo - n*xDiff) | n<-[0..]]]
        where (yOne, xOne) = one
              (yTwo, xTwo) = two
              yDiff = yOne - yTwo
              xDiff = xOne - xTwo

findSymbols :: [[Char]] -> [(Int, Int)] -> Char -> Int -> Int -> [(Int, Int)]
findSymbols stage symbols symbol y x 
        | square == symbol = findSymbols stage ((y, x):symbols) symbol newY newX 
        | end = symbols 
        | otherwise = findSymbols stage symbols symbol newY newX
        where square = if not end then getSquareUnder stage y x else 'm'
              xLength = (length . head) stage
              yLength = length stage
              newX = if x+1 >= xLength then 0 else x+1
              newY = if x+1 >= xLength then y+1 else y
              end = y >= yLength

getSquareUnder stage y x = head $ drop x $ head $ drop y stage

main :: IO()
main = do 
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        print $ solution $ lines contents 