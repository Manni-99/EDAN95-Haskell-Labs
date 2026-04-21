module Sudoku where
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

digitToInt :: Char -> Int
digitToInt x = fromEnum x - fromEnum '0'

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
foldList xss = [x | xs <- xss, x <- xs]

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : [y | y <- removeDuplicates xs, y /= x]

peers :: [(String, [String])]
peers = zip board (map (\a -> filter (/= a) (removeDuplicates (foldList (filterUnitList a)))) board)

fromMaybe :: a -> Maybe a -> a
fromMaybe def Nothing = def
fromMaybe def (Just x) = x

getPeers :: String -> [String]
getPeers s = fromMaybe [] (lookup s peers)

justifyList :: [Maybe a] -> [a]
justifyList [] = []
justifyList (x:xs) =
  case x of
    Nothing -> justifyList xs
    Just v -> v : justifyList xs

lookups :: Eq a => [a] -> [(a, b)] -> [b]
lookups ks table = justifyList [lookup k table | k <- ks]

validSquare :: (String, Int) -> [(String, Int)] -> Bool
validSquare (sq, val) board
  | val == 0  = True
  | otherwise = val `notElem` lookups (getPeers sq) board

validBoard :: [(String, Int)] -> Bool
validBoard board = all (`validSquare` board) board

verifySudoku :: String -> Bool
verifySudoku = validUnits . validBoardNumbers . parseBoard

consistent1 :: String
consistent1 = "1234341221434321"

inconsistent1 :: String
inconsistent1 = "1134341221434321"

consistentWithEmpty :: String
consistentWithEmpty = "1.3434..2143..21"

inconsistentWithEmpty :: String
inconsistentWithEmpty = "11..34..2143..21"

testVerifier :: IO ()
testVerifier = do
  print (verifySudoku consistent1)
  print (verifySudoku inconsistent1)
  print (verifySudoku consistentWithEmpty)
  print (verifySudoku inconsistentWithEmpty)

reduceList :: Eq a => [a] -> [a] -> [a]
reduceList [] ys = []
reduceList xs ys = [x | x <- xs, x `notElem` ys]

validSquareNumbers :: (String, Int) -> [(String, Int)] -> (String, [Int])
validSquareNumbers (sq, v) board
  | v == 0 = (sq, reduceList [1..4] (lookups (getPeers sq) board))
  | validSquare (sq, v) board = (sq, [v])
  | otherwise = (sq, [])

validBoardNumbers :: [(String, Int)] -> [(String, [Int])]
validBoardNumbers board = map (`validSquareNumbers` board) board

b :: [(String, Int)]
b = parseBoard "1.3434..2143..21"
bn :: [(String, [Int])]
bn = validBoardNumbers b

validUnit :: [String] -> [(String, [Int])] -> Bool
validUnit unit boardNums =
  let cands = lookups unit boardNums
  in and [xs /= [] | xs <- cands]
     && and [n `elem` foldList cands | n <- [1..4]]

validUnits :: [(String, [Int])] -> Bool
validUnits boardNums = all (`validUnit` boardNums) unitList
