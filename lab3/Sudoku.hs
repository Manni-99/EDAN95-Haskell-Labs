module Main where

import System.Environment (getArgs)

rows :: String
rows = "ABCDEFGHI"

cols :: String
cols = "123456789"

digits :: [Int]
digits = [1..9]

type Board = [String]

board :: Board
board = cross rows cols


-- Basic helper functions

cross :: [a] -> [a] -> [[a]]
cross xs ys = [[x, y] | x <- xs, y <- ys]

containsElem :: Eq a => a -> [a] -> Bool
containsElem _ [] = False
containsElem e (x:xs)
  | e == x    = True
  | otherwise = containsElem e xs

replacePointsWithZeros :: String -> String
replacePointsWithZeros [] = []
replacePointsWithZeros (x:xs)
  | x == '.'  = '0' : replacePointsWithZeros xs
  | otherwise = x : replacePointsWithZeros xs

digitToInt :: Char -> Int
digitToInt x = fromEnum x - fromEnum '0'

foldList :: [[a]] -> [a]
foldList xss = [x | xs <- xss, x <- xs]

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : [y | y <- removeDuplicates xs, y /= x]

fromMaybe :: a -> Maybe a -> a
fromMaybe def Nothing  = def
fromMaybe _   (Just x) = x

justifyList :: [Maybe a] -> [a]
justifyList [] = []
justifyList (x:xs) =
  case x of
    Nothing -> justifyList xs
    Just v  -> v : justifyList xs

lookups :: Eq a => [a] -> [(a, b)] -> [b]
lookups ks table = justifyList [lookup k table | k <- ks]

reduceList :: Eq a => [a] -> [a] -> [a]
reduceList xs ys = [x | x <- xs, x `notElem` ys]


-- Parsing boards

parseBoard :: String -> [(String, Int)]
parseBoard xs =
    let cleaned = replacePointsWithZeros xs
        nums = map digitToInt cleaned
    in zip board nums


-- Sudoku units and peers

unitList :: [[String]]
unitList = rowUnits ++ colUnits ++ boxUnits
  where
    rowUnits = [cross [r] cols | r <- rows]
    colUnits = [cross rows [c] | c <- cols]

    boxUnits = [cross rs cs | rs <- rowChunks, cs <- colChunks]
      where
        rowChunks = ["ABC", "DEF", "GHI"]
        colChunks = ["123", "456", "789"]

filterUnitList :: String -> [[String]]
filterUnitList square = filter (containsElem square) unitList

peers :: [(String, [String])]
peers =
    zip board
        (map (\a -> filter (/= a)
              (removeDuplicates (foldList (filterUnitList a)))) board)

getPeers :: String -> [String]
getPeers s = fromMaybe [] (lookup s peers)


validSquare :: (String, Int) -> [(String, Int)] -> Bool
validSquare (sq, val) boardValues
  | val == 0  = True
  | otherwise = val `notElem` lookups (getPeers sq) boardValues

validSquareNumbers :: (String, Int) -> [(String, Int)] -> (String, [Int])
validSquareNumbers (sq, v) boardValues
  | v == 0 = (sq, reduceList digits (lookups (getPeers sq) boardValues))
  | validSquare (sq, v) boardValues = (sq, [v])
  | otherwise = (sq, [])

validBoardNumbers :: [(String, Int)] -> [(String, [Int])]
validBoardNumbers boardValues =
    map (`validSquareNumbers` boardValues) boardValues

validUnit :: [String] -> [(String, [Int])] -> Bool
validUnit unit boardNums =
  let cands = lookups unit boardNums
  in and [xs /= [] | xs <- cands]
     && and [n `elem` foldList cands | n <- digits]

validUnits :: [(String, [Int])] -> Bool
validUnits boardNums =
    all (`validUnit` boardNums) unitList

verifySudoku :: String -> Bool
verifySudoku =
    validUnits . validBoardNumbers . parseBoard


simpleConflictSquares :: [(String, Int)] -> [String]
simpleConflictSquares boardValues =
    [sq | (sq, v) <- boardValues,
          v /= 0,
          not (validSquare (sq, v) boardValues)]

blockedConflicts :: [(String, Int)] -> [([String], Int)]
blockedConflicts boardValues =
    let boardNums = validBoardNumbers boardValues
    in [(unit, n) | unit <- unitList,
                    n <- digits,
                    n `notElem` foldList (lookups unit boardNums)]

blockedConflictSquares :: [(String, Int)] -> [String]
blockedConflictSquares boardValues =
    removeDuplicates [sq | (unit, _) <- blockedConflicts boardValues, sq <- unit]

showUnit :: [String] -> String
showUnit unit =
    foldr1 (\a b -> a ++ " " ++ b) unit

printBlockedConflicts :: [(String, Int)] -> IO ()
printBlockedConflicts boardValues =
    mapM_ printOneConflict (blockedConflicts boardValues)
  where
    printOneConflict :: ([String], Int) -> IO ()
    printOneConflict (unit, n) =
        putStrLn ("Blocked conflict: number "
                  ++ show n
                  ++ " cannot be placed in unit "
                  ++ showUnit unit)

testInvalidSudoku :: String
testInvalidSudoku = "553..7...6..195....98....6.8...6...34..8.3..17...2...6.6....28....419..5....8..79"
testValidSudoku :: String
testValidSudoku = "53..7....6..195....98....6.8...6...34..8.3..17...2...6.6....28....419..5....8..79"
blockedSudoku :: String
blockedSudoku = "..345678912......................................................................"
-- Pretty printing

printSudoku :: [(String, Int)] -> IO ()
printSudoku boardValues = do
    putStrLn border
    mapM_ printRow rows
  where
    border :: String
    border = "+-----+-----+-----+-----+-----+-----+-----+-----+-----+"

    simpleSquares :: [String]
    simpleSquares = simpleConflictSquares boardValues

    blockedSquares :: [String]
    blockedSquares = blockedConflictSquares boardValues

    printRow :: Char -> IO ()
    printRow r = do
        putStr "|"
        mapM_ (printSquare r) cols
        putStrLn ""
        putStrLn border

    printSquare :: Char -> Char -> IO ()
    printSquare r c =
        let sq = [r, c]
        in case lookup sq boardValues of
            Just 0 ->
                if sq `elem` blockedSquares
                then putStr "  ?  |"
                else putStr "     |"

            Just n ->
                if sq `elem` simpleSquares
                then putStr (" !" ++ show n ++ "! |")
                else putStr ("  " ++ show n ++ "  |")

            Nothing ->
                putStr " ??? |"


-- Reading Sudoku files

onlySudokuChars :: String -> String
onlySudokuChars xs =
    [x | x <- xs, x `elem` ".0123456789"]

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs =
    take n xs : splitEvery n (drop n xs)

readSudokus :: String -> [String]
readSudokus fileContent =
    let chars = onlySudokuChars fileContent
    in filter (\s -> length s == 81) (splitEvery 81 chars)


-- Checking all boards from a file

checkSudokus :: Int -> [String] -> IO ()
checkSudokus _ [] = return ()
checkSudokus n (s:ss) = do
    putStrLn ("Sudoku " ++ show n ++ ":")

    if verifySudoku s
    then putStrLn "Valid"
    else do
        putStrLn "Invalid"
        printSudoku (parseBoard s)
        printBlockedConflicts (parseBoard s)

    putStrLn ""
    checkSudokus (n + 1) ss


-- Main program

main :: IO ()
main = do
    args <- getArgs

    case args of
        [filename] -> do
            content <- readFile filename
            let sudokus = readSudokus content

            putStrLn ("Found " ++ show (length sudokus) ++ " Sudoku board(s).")
            putStrLn ""

            checkSudokus 1 sudokus

        _ ->
            putStrLn "Usage: runhaskell Main.hs filename.txt"