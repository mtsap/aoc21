import Control.Arrow ((&&&))
import Data.Foldable (Foldable (foldl'))
import Data.List.Utils
import Data.Monoid

avg :: [Int] -> Int
avg pos =
  let a = foldMap (\x -> (Sum x, Sum 1)) pos
   in round $ (fromIntegral . getSum $ fst a) / (fromIntegral . getSum $ snd a)

crabFuelConsumption1 :: Int -> Int -> Int
crabFuelConsumption1 start finish = abs (start - finish)

crabFuelConsumption2 :: Int -> Int -> Int
crabFuelConsumption2 start finish = foldl' (+) 0 [1 .. abs (finish - start)]

calcFuelConsumption :: (Int -> Int -> Int) -> [Int] -> Int -> Int
calcFuelConsumption consumptionFunc allPositions pos = foldl' (+) 0 $ map (`consumptionFunc` pos) allPositions

part1 :: [Int] -> Int
part1 positions = minimum $ map (calcFuelConsumption crabFuelConsumption1 positions) [minimum positions .. maximum positions]

part2 :: [Int] -> Int
part2 positions = minimum $ map (calcFuelConsumption crabFuelConsumption2 positions) [minimum positions .. maximum positions]

prepare = map (\x -> read x :: Int) . words . replace "," " "
main :: IO ()
main = readFile "./input.txt" >>= print . (part1 &&& part2) . prepare
