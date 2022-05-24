main =  do
  contents <- readFile "./q1/input.txt"
  let readings = map readAsInt . lines $ contents
  -- let myData = drop 2 readings
  -- let myDataShift = drop 1 readings
  -- let myData2Shifts =   readings 

  let sums = zipWith3 sum3 ( drop 2 readings ) ( drop 1 readings ) readings
  let sumsShifted = 0 : sums
  let isLarger =  filter (== True) $ drop 1 $ zipWith (>) sums sumsShifted
  
  -- print . take 10 $ myData 
  -- print . take 10 $ myDataShift 
  -- print . take 10 $ myData2Shifts 
  print . take 10 $ sums 
  print . take 10 $ sumsShifted 
  print . take 10 $ isLarger 
  print . length $ isLarger

readAsInt :: String -> Int
readAsInt = read

sum3 :: Int -> Int -> Int -> Int 
sum3 a b c = a + b + c

