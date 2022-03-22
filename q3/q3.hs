import Data.List
import Text.XHtml.Transitional (stringToHtml)
import Data.Char (digitToInt)

main =  do
  contents <- readFile "./q3/testData.txt"
  let myData = lines contents
  let input =map strToListOfInt myData
  let transposed = transpose myData
  let sums = map ( sum . strToListOfInt ) transposed 
  let dataLength = length myData
  let wordLength = length . head $ myData
  let gammaStr = map ( filterFor dataLength ) sums
  let epsilonStr = map not gammaStr

  let res = foldl' (+) 0 input
  print input
  


readAsInt :: String -> Int
readAsInt = read

fst3 :: (a,b,c) -> a
fst3 ( a, _ , _ ) = a

snd3 :: (a,b,c) -> b
snd3 ( _ , b , _ ) = b

strToListOfInt :: String -> [Int]
strToListOfInt = map (read . (:""))

fromBoolToBit :: Bool -> Char
fromBoolToBit True = '1'
fromBoolToBit False = '0'

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

filterFor :: Int -> Int -> Bool
filterFor limit value
  | value * 2 > limit = True
  | value * 2 == limit = True
  | value * 2 < limit = False
  | otherwise = False
