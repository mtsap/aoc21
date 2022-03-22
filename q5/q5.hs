import qualified Data.Map as Map
import qualified Data.Map.Internal.Debug as MapD

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = Map.toList (Map.fromListWith (+) [(x, 1) | x <- xs])

isHead' :: String -> String -> String -> Bool
isHead' str [] ret = True
isHead' [] key ret = False
isHead' (x : xs) (x' : xs') ret = (x == x') && isHead' xs xs' ret

isHead :: String -> String -> Bool
isHead key str = isHead' str key key

remouveXelem :: Int -> [a] -> [a]
remouveXelem i a | i <= 0 = a
remouveXelem _ [] = []
remouveXelem i (x : xs) = remouveXelem (i - 1) xs

replace :: String -> String -> String -> String
replace ori new [] = []
replace ori new s =
  if isHead ori s
    then new ++ replace ori new (remouveXelem (length ori) s)
    else head s : replace ori new (tail s)

split :: Char -> String -> [String]
split c xs = case break (== c) xs of
  (ls, "") -> [ls]
  (ls, x : rs) -> ls : split c rs

data Point = Point Int Int deriving (Show, Eq, Ord)
data Line = Line Point Point deriving (Show, Eq)

getDiagLine :: Line -> [Point]
getDiagLine (Line (Point a b) (Point c d)) =
  zipWith Point (getRs a c) (getRs b d)
 where
  getRs x1 x2 =
    if x1 > x2
      then [x1, x1 - 1 .. x2]
      else [x1 .. x2]

lineToPoints :: Line -> [Point]
lineToPoints (Line (Point a b) (Point c d))
  | a == c && b > d = [Point a i | i <- [d .. b]]
  | a == c && b < d = [Point a i | i <- [b .. d]]
  | a > c && b == d = [Point i b | i <- [c .. a]]
  | a < c && b == d = [Point i b | i <- [a .. c]]
  | abs (a - c) == abs (b - d) = getDiagLine (Line (Point a b) (Point c d))
  | a /= c && b /= d = []
  | a == c && b == d = [Point a b]
lineToPoints (Line (Point _ _) (Point _ _)) = []

mkLine :: [String] -> Line
-- mkLine (a : b : c : d : xs) = Line (Point (read a :: Int) (read b :: Int)) (Point (read c :: Int) (read d :: Int))
mkLine (a : b : c : d : xs) = Line (Point (read a) (read b)) (Point (read c) (read d))

main :: IO ()
main = do
  -- contents <- readFile "./input.txt"
  -- let points = concatMap (lineToPoints . mkLine . split ',' . replace " -> " ",") $ lines contents
  -- let freqs = length . filter (\(_, times) -> times > 1) . frequency $ points
  -- print freqs
  let a = Map.fromList [(5 :: Int, ["5"]), (3 :: Int, ["3"])]
  let b = Map.insertWith (<>) 5 ["4"] a
  print b
