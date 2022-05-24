import Control.Arrow ((&&&))
import Control.Monad (replicateM)
import Data.Char (toLower)
import Data.Foldable (Foldable (foldl'), asum)
import Data.List.Utils
import Data.Maybe
import Data.Monoid
import Debug.Trace
import GHC.Generics (Generic (from))
import Text.Regex.Applicative
import Text.Regex.Applicative.Common (decimal)

data Segment = A | B | C | D | E | F | G deriving (Enum, Show, Read, Eq, Ord, Bounded)
newtype Digit = Digit {segments :: [Segment]} deriving (Show)
data Display = Display
  { allPatterns :: [Digit]
  , reading :: [Digit]
  }
  deriving (Show)

type Input = [Display]

isIdentifiable :: Digit -> Bool
isIdentifiable d = case segmentsOn d of
  2 -> True
  3 -> True
  4 -> True
  7 -> True
  _ -> False

segmentsOn :: Digit -> Int
segmentsOn = length . segments

part1 :: [Display] -> Int
part1 = length . filter (== True) . map isIdentifiable . concatMap reading

part2 :: [Display] -> [Display]
part2 = id

type Regex a = RE Char a

segment :: Regex Segment
segment = asum $ do
  seg <- [minBound .. maxBound]
  pure $ seg <$ sym (toLower (head (show seg)))

segment1 :: [Maybe Segment]
segment1 = do
  a <- [minBound :: Segment .. maxBound]
  pure $ a <$ Just (toLower (head (show a)))

--
-- segment :: RE Char Segment
-- segment = fmap read sym 'a'

digit :: Regex Digit
digit = Digit <$> many segment

display :: Regex Display
display = Display <$> replicateM 10 (digit <* sym ' ') <* sym '|' <*> replicateM 4 (sym ' ' *> digit)

prepare :: String -> Input
prepare s = fromMaybe [] $ match (many (display <* sym '\n')) s

main :: IO ()
-- main = readFile "./input.txt" >>= print . (part1 &&& part2) . prepare
main = readFile "./input.txt" >>= print . part1 . prepare
