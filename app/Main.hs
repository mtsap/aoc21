module Main where

import Control.Arrow ((&&&))
import Control.Monad (replicateM)
import Data.Char (isUpper, toUpper)
import Data.Maybe
import Text.Regex.Applicative
import Text.Regex.Applicative.Common (decimal)

type Input = [Path]
type Regex a = RE Char a

-- // TODO
part1 :: Input -> Input
part1 = id

-- // TODO
part2 :: Input -> Input
part2 = id

data Cave = START | END | Big [Char] | Small [Char] deriving (Show, Eq, Read)
type Path = (Cave, Cave)

start :: Regex Cave
start = read . map toUpper <$> replicateM 5 anySym

end :: Regex Cave
end = read . map toUpper <$> replicateM 3 anySym

middleCave :: Regex Cave
middleCave = (\(c : cs) -> if isUpper c then Big (c : cs) else Small (c : cs)) <$> replicateM 2 anySym

cave :: Regex Cave
-- cave = Cave <$> (toUpper $ (replicateM 3 anySym)) <|> toUpper . (replicateM 5 anySym) <|> isUpper
cave = middleCave <|> end <|> start

path :: Regex Path
path = (,) <$> cave <* sym '-' <*> cave

prepare :: String -> [Path]
prepare s = fromMaybe [] $ match (some (path <* sym '\n')) s

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
