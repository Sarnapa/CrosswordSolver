import System.Environment
import Data.List

{-
Michał Piotrak
Paweł Piotrowski
Jędrzej Kalisiak
-}



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

-- Szukanko
firsts :: [(a,b)] -> [a]
firsts ps = [x | (x,_) <- ps]

substrIdx :: Eq a => [a] -> [(a, b)] -> [(a, b)]
substrIdx _ [] = []
substrIdx sub str = case (findIndex (isPrefixOf sub) (tails (firsts str))) of
        Just n -> (drop n (take (n + length sub) str))
        Nothing -> []
        
searchWord :: [Char] -> [[(Char,b)]] -> [[(Char,b)]]
searchWord _ [] = []
searchWord word board = map (substrIdx word) board

search :: [[Char]] -> [[(Char,b)]] -> [[(Char,b)]]
search [] _ = []
search (x:xs) board = searchWord x board ++ search xs board

-- Wyszukiwanie zakreślonych pół, zastępywanie ich domyślnym znaczkiem

reduceToText :: Eq b => [[(Char,b)]] -> [(Char,b)] -> [Char] 
reduceToText [] _ = []
reduceToText boardrows foundchars =  firsts ((concat boardrows) \\ foundchars)


--Zamien znak-pole jesli na liscie
reduceMarkedInRowHelp1 :: [(Char, Pos)] -> (Char, Pos) -> (Char, Pos)
reduceMarkedInRowHelp1 charsToReplace char = case find (==char) charsToReplace of
        Just (c, bb) -> ('░', bb)
        Nothing -> char

--podmień w jednym rzędzie
reduceMarkedInRow :: [(Char, Pos)] -> [(Char, Pos)] -> [(Char, Pos)] 
reduceMarkedInRow [] _ = []
reduceMarkedInRow foundchars maprow = map (reduceMarkedInRowHelp1 foundchars) maprow

--podmien w całej planszy
reduceMarkedInBoard :: [(Char, Pos)] -> [[(Char, Pos)]] -> [[(Char, Pos)]] 
reduceMarkedInBoard foundchars maprows = map (reduceMarkedInRow foundchars) maprows

printDotsInit :: Int -> [Char]
printDotsInit n
                | n<=0 = []
                | otherwise = "├─" ++ printDots (n-1)

printDots :: Int -> [Char]
printDots n 
            | n==0 = "┤"
            | otherwise ="┼─" ++ printDots (n-1)

printBoardInit :: [[(Char, Pos)]] -> [Char]
printBoardInit [] = []
printBoardInit rows = printDotsInit (length (head rows)) ++ ['\n'] ++ printBoard (map firsts rows)

printBoard :: [[Char]] -> [Char]
printBoard [row] = printBoardRow row ++ ['\n'] ++ printDotsInit (length row) ++ ['\n'] 
printBoard (row:rows) = printBoard [row] ++ printBoard rows

printBoardRow :: [Char] -> [Char]
printBoardRow [c] =  "│" ++ [c] ++ "│"
printBoardRow (c:cs) =  "│" ++ [c] ++ printBoardRow cs

deduplicate :: Eq a => [a] -> [a]
deduplicate []       = []
deduplicate (x : xs) = x : deduplicate (filter (x /=) xs)



main = do
  putStrLn "Welcome to CrosswordSolver"

  -- Pobranie nazw plikow wejsciowych z argumentow wywolania programu
  (wordsFileName:boardFileName:_) <- getArgs
  wordsFile <- readFile wordsFileName
  boardFile <- readFile boardFileName

  putStrLn "Given words:"
  putStrLn wordsFile
  putStrLn ""


  let wordsTab = lines wordsFile
  let boardRows = getBoardRowsInit boardFile
  let boardRowsCount = length(boardRows)
  let boardColumnsCount = length(boardRows !! 0)
  let boardCols = getBoardColumnsInit boardRows boardColumnsCount

  let boardReversedRows = reverse boardRows
  let boardReversedCols = getReversedCols boardCols
  let boardDiagonals = getBoardDiagonalsInit boardRows boardCols boardRowsCount boardColumnsCount ++ getBoardDiagonalsInit boardReversedRows boardReversedCols boardRowsCount boardColumnsCount

  -- do wypisania przekatnych
  let boardAll = boardRows ++ boardCols ++ boardDiagonals
  
  let markedFields_ = filter (not . null) (search wordsTab boardAll) -- zakreslone pola

  let markedFields = deduplicate (concat markedFields_) -- lista zakreślonych pół redukcja sublist, po deduplikacji w celu zwiększenia wydajności
 
  putStrLn "Given board:"
  putStrLn (printBoardInit boardRows)
  
  putStrLn "Result:"
  
  let boardRowsCleaned = reduceMarkedInBoard markedFields boardRows
  putStrLn (printBoardInit boardRowsCleaned)
  
  putStrLn "The word is:"
  putStrLn (reduceToText boardRows markedFields)
  
  putStrLn "\nBye!"
  
