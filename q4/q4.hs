import Data.List 
import Data.List.Split
import Data.Maybe

main :: IO ()
main = do
  contents <- readFile "./input.txt"
  let myLines = lines contents
  let drawNumbers = map readInt . splitOn "," . head $ myLines
  let rows = map  ( map readInt . words ) . filter (/= "") . drop 2 $ myLines
  let columns = concatMap transpose . chunks 5 $ rows
  let numOfBoards = length . chunks 5 $ rows
  let finalState = until checkWin nextDraw (rows, columns, drawNumbers, -1, ( take numOfBoards [False, False ..], take numOfBoards [False, False ..] ))
  let lastDrawn = frth finalState
  let rowsIndex = elemIndex [] . fst3 $ finalState
  let colsIndex = elemIndex [] . snd3 $ finalState
  let index = div ( head . catMaybes $ [rowsIndex, colsIndex] ) 5
  let lastWinningIndex = fromJust . elemIndex False . fst . fifth $ finalState
  let lastWinningBoard =  chunks 5  ( fst3 finalState ) !! lastWinningIndex
  let s = sum . concat $ lastWinningBoard
  print lastDrawn
  print numOfBoards
  print lastWinningIndex 
  print (s * lastDrawn )  


readInt :: String -> Int
readInt = read

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n l
  | n > 0 = take n l : chunks n (drop n l)
  | otherwise = error "Negative or zero n"

fst3 (a, _ , _ ,_ ,_) = a
snd3 ( _ , a, _ ,_ ,_) = a
trd3 ( _ , _, a ,_ ,_) = a
frth ( _ , _,_ ,a ,_) = a
fifth ( _ , _,_ ,_ ,a) = a

checkWin :: ( [[Int]] , [[Int]] , [Int] , Int , ( [Bool] ,[Bool]) ) -> Bool
checkWin (rows, columns , _ , _ , _) =   and ( zipWith (||)  ( map ( elem [] ) . chunks 5 $ columns  ) ( map ( elem [] ) . chunks 5 $ rows ) )

nextDraw :: ( [[Int]] , [[Int]] , [Int] , Int , ( [Bool] ,[Bool]) ) -> ( [[Int]] , [[Int]] , [Int] , Int , ( [Bool] ,[Bool]) )
nextDraw (rows, columns, drawNunbers,  drawnNumber, (curr, prev)) = 
  (
    map (delete . head $ drawNunbers) rows,
    map (delete . head $ drawNunbers) columns,
    drop 1 drawNunbers,
    head drawNunbers,
    (  zipWith (||)  ( map ( elem [] ) . chunks 5 $ columns  ) ( map ( elem [] ) . chunks 5 $ rows ) , curr)
  )
