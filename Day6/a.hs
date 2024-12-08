import System.IO
import Data.List

solution xs = length $ nub $ keepMovingInHerGuard blockers [] (y, x, '^') (yLength, xLength) -- nub $ keepMovingInHerGuard xs []
        where (y, x) = findGuard xs
              blockers = findBlockers xs [] 0 0
              xLength = (length . head) xs
              yLength = length xs 

keepMovingInHerGuard :: [(Int, Int)] -> [(Int, Int)] -> (Int, Int, Char) -> (Int, Int) -> [(Int, Int)]
keepMovingInHerGuard blockers moves guard bounds = if oob then (y,x):moves else keepMovingInHerGuard blockers ((y,x):moves) (move blockers guard) bounds
        where (y, x, rotation) = guard
              (yFront, xFront) = inFront guard
              (yLength, xLength) = bounds
              oob = xFront < 0 || yFront < 0 || xFront >= xLength || yFront >= yLength

move :: [(Int, Int)] -> (Int, Int, Char) -> (Int, Int, Char)
move blockers guard = if (yFront, xFront) `elem` blockers then (yGuard, xGuard, newRotation) else (yFront, xFront, rotation)
        where (yGuard, xGuard, rotation) = guard
              (yFront, xFront) = inFront guard
              newRotation = turn guard

findBlockers :: [[Char]] -> [(Int, Int)] -> Int -> Int -> [(Int, Int)]
findBlockers stage blockers y x 
        | square == '#' = findBlockers stage ((y, x):blockers) newY newX 
        | end = blockers 
        | otherwise = findBlockers stage blockers newY newX
        where square = if not end then getSquareUnder stage y x else 'm'
              xLength = (length . head) stage
              yLength = length stage
              newX = if x+1 >= xLength then 0 else x+1
              newY = if x+1 >= xLength then y+1 else y
              end = y >= yLength

turn :: (Int, Int, Char) -> Char
turn guard 
        | rotation == '^' = '>'
        | rotation == 'v' = '<'
        | rotation == '<' = '^'
        | rotation == '>' = 'v'
        where (y, x, rotation) = guard

replaceSquare :: [[Char]] -> Char -> Int -> Int -> [[Char]]
replaceSquare stage new y x = oldTop ++ newRow:oldBottom
        where oldTop = take y stage
              oldBottom = drop (y+1) stage
              oldLeft = take x $ head $ drop y stage
              oldRight = drop (x+1) $ head $ drop y stage
              newRow = oldLeft ++ new:oldRight
inFront :: (Int, Int, Char) -> (Int, Int)
inFront guard 
        | rotation == '^' = (y-1, x)
        | rotation == 'v' = (y+1, x)
        | rotation == '<' = (y, x-1)
        | rotation == '>' = (y, x+1)
        where (y, x, rotation) = guard

getSquareUnder stage y x = head $ drop x $ head $ drop y stage

getDirection stage = getSquareUnder stage y x
        where (y, x) = findGuard stage

findGuard :: [[Char]] -> (Int, Int)
findGuard stage =  (y, x)
        where (before, after) = break (any (\x -> x/='.' && x/='#')) stage
              y = length before
              foundRow = head after
              x = length $ takeWhile (\x -> x=='.' || x=='#') foundRow

main :: IO()
main = do 
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        print $ solution $ lines contents 