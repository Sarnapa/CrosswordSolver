import System.Environment

-- Pozycja na planszy
type Pos = (Int, Int)

getBoardRow :: String -> Int -> Int -> [(Char, Pos)]
getBoardRow [] _ _ = []
getBoardRow (letter:letters) rowIdx columnIdx = (letter, (rowIdx, columnIdx)) : getBoardRow letters rowIdx (columnIdx + 1)

getBoardRows :: [String] -> Int -> [[(Char, Pos)]]
getBoardRows [] _ = []
getBoardRows (row:rows) rowIdx = getBoardRow row rowIdx 0 : getBoardRows rows (rowIdx + 1)

-- Plansza jako zbiór wierszy
getBoardRowsInit :: String -> [[(Char, Pos)]]
getBoardRowsInit boardString = getBoardRows (lines boardString) 0

getBoardColumn :: [[(Char, Pos)]] -> Int -> [(Char, Pos)]
getBoardColumn [] _ = []
getBoardColumn (row:rows) colIdx = (row !! colIdx) : getBoardColumn rows colIdx

getBoardColumns :: [[(Char, Pos)]] -> Int -> Int -> [[(Char, Pos)]]
getBoardColumns rows colIdx colsCount =
  if colIdx /= colsCount then
    getBoardColumn rows colIdx : getBoardColumns rows (colIdx + 1) colsCount
  else
    []

-- Plansza jako zbiór kolumn
getBoardColumnsInit :: [[(Char, Pos)]] -> Int -> [[(Char, Pos)]]
getBoardColumnsInit rows colsCount = getBoardColumns rows 0 colsCount

getBoardDiagonal :: [[(Char, Pos)]] -> Int -> Int -> [(Char, Pos)]
getBoardDiagonal [] _ _ = []
getBoardDiagonal (letters:lettersMatrix) letterIdx limit =
  if letterIdx /= limit then
    (letters !! letterIdx) : getBoardDiagonal lettersMatrix (letterIdx + 1) limit
  else
    []

getBoardDiagonals :: [[(Char, Pos)]] -> Int -> Int -> [[(Char, Pos)]]
getBoardDiagonals lettersMatrix letterIdx limit =
  if letterIdx /= limit then
    getBoardDiagonal lettersMatrix letterIdx limit : getBoardDiagonals lettersMatrix (letterIdx + 1) limit
  else
    []

-- Plansza jako zbiór przekątnych (z kolumn zbieramy od 1, zeby nie powtarzala sie glowna przekatna) - przekatne "z gory"
getBoardDiagonalsInit :: [[(Char, Pos)]] -> [[(Char, Pos)]] -> Int -> Int -> [[(Char, Pos)]]
getBoardDiagonalsInit rows cols rowsCount colsCount = getBoardDiagonals rows 0 colsCount ++ getBoardDiagonals cols 1 rowsCount

-- Odwracanie kolejnosci liter w kolumnach
getReversedCols :: [[(Char, Pos)]] -> [[(Char, Pos)]]
getReversedCols [] = []
getReversedCols (col:cols) = reverse col : getReversedCols cols

main = do
  putStrLn "Welcome to CrosswordSolver"

  -- Pobranie nazw plikow wejsciowych z argumentow wywolania programu
  (wordsFileName:boardFileName:_) <- getArgs
  wordsFile <- readFile wordsFileName
  boardFile <- readFile boardFileName

  putStrLn "Given words:"
  putStrLn wordsFile
  putStrLn ""
  putStrLn "Given board:"
  putStrLn boardFile

  let wordsTab = lines wordsFile
  let boardRows = getBoardRowsInit boardFile
  let boardRowsCount = length(boardRows)
  let boardColumnsCount = length(boardRows !! 0)
  let boardCols = getBoardColumnsInit boardRows boardColumnsCount

  let boardReversedRows = reverse boardRows
  let boardReversedCols = getReversedCols boardCols
  let boardDiagonals = getBoardDiagonalsInit boardRows boardCols boardRowsCount boardColumnsCount ++ getBoardDiagonalsInit boardReversedRows boardReversedCols boardRowsCount boardColumnsCount
  -- do wypisania przekatnych
  mapM_ print boardDiagonals

  putStrLn "Result:"
