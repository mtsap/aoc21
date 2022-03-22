import Data.Tuple

main =  do
  contents <- readFile "./q2/input.txt"
  let initialPosition = (0,0)
  let movements = map (( \( x:y:xs ) -> (parseDir x, readAsInt y)) . words) $ lines contents
  
  let finalPos = foldl calcNextPos (0,0,0) movements


  let aaa =  (*) (fst3 finalPos) (snd3 finalPos) 

  print  aaa

readAsInt :: String -> Int
readAsInt = read

sum3 :: Int -> Int -> Int -> Int 
sum3 a b c = a + b + c

parseDir :: String -> Int
parseDir "down" = 1
parseDir "up" = -1
parseDir _ = 0

calcNextPos :: (Int,Int, Int) -> (Int,Int) -> (Int,Int, Int)
calcNextPos (x,y,aim) (0, d) = ( x + d , y + (d  * aim), aim ) 
calcNextPos (x,y,aim) (1, d) = ( x , y, aim + d) 
calcNextPos (x,y,aim) (-1, d) = ( x ,y, aim - d) 
calcNextPos (x,y,aim) (_, d) = ( x , y, aim) 


fst3 :: (a,b,c) -> a
fst3 ( a, _ , _ ) = a

snd3 :: (a,b,c) -> b
snd3 ( _ , b , _ ) = b
