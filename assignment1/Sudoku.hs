module Main where

-- Instructions:
-- Put conflicts.txt, blockings.txt, easy50.txt, and inconsistent20.txt
-- in the same folder as this file.
--
-- Run with:
--   runhaskell Sudoku.hs
--
-- Names:
--   Kristmann Thorsteinsson

rows :: String
rows = "ABCDEFGHI"

cols :: String
cols = "123456789"

digits :: [Int]
digits = [1..9]

type Board = [String]

board :: Board
board = cross rows cols

cross :: [a] -> [a] -> [[a]]
cross xs ys = [[x, y] | x <- xs, y <- ys]

containsElem :: Eq a => a -> [a] -> Bool
containsElem _ [] = False
containsElem e (x:xs)
  | e == x    = True
  | otherwise = containsElem e xs

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

isSudokuChar :: Char -> Bool
isSudokuChar c = c `elem` ".0123456789"

replaceDotWithZero :: Char -> Char
replaceDotWithZero c
  | c == '.'  = '0'
  | otherwise = c

cleanSudokuString :: String -> String
cleanSudokuString xs =
    [replaceDotWithZero x | x <- xs, isSudokuChar x]

parseBoard :: String -> [(String, Int)]
parseBoard xs =
    let cleaned = cleanSudokuString xs
        nums = map digitToInt cleaned
    in zip board nums

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
        (map (\sq -> filter (/= sq)
              (removeDuplicates (foldList (filterUnitList sq)))) board)

getPeers :: String -> [String]
getPeers sq = fromMaybe [] (lookup sq peers)

validSquare :: (String, Int) -> [(String, Int)] -> Bool
validSquare (sq, val) boardValues
  | val == 0  = True
  | otherwise = val `notElem` lookups (getPeers sq) boardValues

validSquareNumbers :: (String, Int) -> [(String, Int)] -> (String, [Int])
validSquareNumbers (sq, val) boardValues
  | val == 0 =
      (sq, reduceList digits (lookups (getPeers sq) boardValues))

  | validSquare (sq, val) boardValues =
      (sq, [val])

  | otherwise =
      (sq, [])

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
verifySudoku s =
    length cleaned == 81
    && validUnits (validBoardNumbers (parseBoard cleaned))
  where
    cleaned = cleanSudokuString s


simpleConflictSquares :: [(String, Int)] -> [String]
simpleConflictSquares boardValues =
    [sq | (sq, val) <- boardValues,
          val /= 0,
          not (validSquare (sq, val) boardValues)]

simpleConflictUnits :: [(String, Int)] -> [([String], Int)]
simpleConflictUnits boardValues =
    [(unit, n) | unit <- unitList,
                 n <- digits,
                 count n (lookups unit boardValues) > 1]

blockedConflicts :: [(String, Int)] -> [([String], Int)]
blockedConflicts boardValues =
    let boardNums = validBoardNumbers boardValues
    in [(unit, n) | unit <- unitList,
                    n <- digits,
                    n `notElem` foldList (lookups unit boardNums)]

blockedConflictSquares :: [(String, Int)] -> [String]
blockedConflictSquares boardValues =
    removeDuplicates [sq | (unit, _) <- blockedConflicts boardValues,
                           sq <- unit]

count :: Eq a => a -> [a] -> Int
count _ [] = 0
count x (y:ys)
  | x == y    = 1 + count x ys
  | otherwise = count x ys

showUnit :: [String] -> String
showUnit [] = ""
showUnit [x] = x
showUnit (x:xs) = x ++ " " ++ showUnit xs

printSimpleConflicts :: [(String, Int)] -> IO ()
printSimpleConflicts boardValues =
    mapM_ printOneConflict (simpleConflictUnits boardValues)
  where
    printOneConflict :: ([String], Int) -> IO ()
    printOneConflict (unit, n) =
        putStrLn ("Direct conflict: number "
                  ++ show n
                  ++ " appears more than once in unit "
                  ++ showUnit unit)

printBlockedConflicts :: [(String, Int)] -> IO ()
printBlockedConflicts boardValues =
    mapM_ printOneConflict (blockedConflicts boardValues)
  where
    printOneConflict :: ([String], Int) -> IO ()
    printOneConflict (unit, n) =
        putStrLn ("Blocking conflict: number "
                  ++ show n
                  ++ " cannot be placed in unit "
                  ++ showUnit unit)


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


splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs =
    take n xs : splitEvery n (drop n xs)

readSudokus :: String -> [String]
readSudokus fileContent =
    let chars = cleanSudokuString fileContent
    in filter (\s -> length s == 81) (splitEvery 81 chars)


checkSudokus :: Int -> [String] -> IO ()
checkSudokus _ [] = return ()
checkSudokus n (s:ss) = do
    putStrLn ("Sudoku " ++ show n ++ ":")

    if verifySudoku s
    then putStrLn "Valid"
    else do
        let parsed = parseBoard s
        putStrLn "Invalid"
        printSudoku parsed
        printSimpleConflicts parsed
        printBlockedConflicts parsed

    putStrLn ""
    checkSudokus (n + 1) ss

testFile :: String -> IO ()
testFile filename = do
    putStrLn ("Testing file: " ++ filename)
    content <- readFile filename

    let sudokus = readSudokus content

    putStrLn ("Found " ++ show (length sudokus) ++ " Sudoku board(s).")
    putStrLn ""

    checkSudokus 1 sudokus


-- Main program

main :: IO ()
main = do
    testFile "conflicts.txt"
    testFile "blockings.txt"
    testFile "easy50.txt"
    testFile "inconsistent20.txt"