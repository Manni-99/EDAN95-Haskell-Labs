module Sudoku where
import Data.Char
import Data.List
import Distribution.FieldGrammar (List)

rows :: String
rows = "ABCD"
cols :: String
cols = "1234"

containsElem :: (Eq a) => a -> [a] -> Bool
containsElem _ [] = False
containsElem elem (x : xs)
  | elem == x = True
  | otherwise = containsElem elem xs

cross :: [a] -> [a] -> [[a]]
cross xs ys = [[x, y] | x <- xs, y <- ys]

replacePointsWithZeros :: [Char] -> [Char]
replacePointsWithZeros [] = []
replacePointsWithZeros (x : xs)
  | x == '.' = '0' : replacePointsWithZeros xs
  | otherwise = x : replacePointsWithZeros xs

type Board = [String]

board :: Board
board = cross rows cols

boardString :: String
boardString = "53..7....6..195....98....6.8...6...34..8.3..17...2...6.6....28....419..5....8..79"

parsedZeros :: String
parsedZeros = replacePointsWithZeros boardString

digitize :: [Int]
digitize = map digitToInt parsedZeros

parseBoard :: String -> [(String, Int)]
parseBoard xs = 
    let step1 = cross rows cols
        step2 = replacePointsWithZeros xs
        step3 = map digitToInt step2
    in zip step1 step3


unitList :: [[String]]
unitList = rowUnits ++ colUnits ++ boxUnits
    where 
      rowUnits = [cross [r] cols | r <- rows] 
      colUnits = [cross rows [c] | c <- cols]
      boxUnits = [cross rs cs | rs <- rowChunks, cs <- colChunks]
        where
          rowChunks = ["AB", "CD"]
          colChunks = ["12", "34"]

filterUnitList :: String -> [[String]]
filterUnitList square = filter (containsElem square) unitList

units :: [(String, [[String]])]
units = 
    let step1 = map filterUnitList board 
    in zip board step1 

foldList :: [[a]] -> [a]
foldList = concat 

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = nub

peers :: [(String, [String])]
peers = zip board (map (\a -> filter (/= a) (removeDuplicates (foldList (filterUnitList a)))) board)
