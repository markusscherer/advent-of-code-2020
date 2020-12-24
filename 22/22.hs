import Data.List
import Data.Maybe
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug

type Parser = Parsec Void String

play (s, []) = s
play ([], s) = s
play ((x : xs), (y : ys))
  | x > y = play ((xs ++ [x, y]), ys)
  | y > x = play (xs, (ys ++ [y, x]))

solve1 l = sum $ zipWith (*) [1 ..] (reverse $ play l)

main = do
  input <- getContents
  let l = fromJust $ parseMaybe parseInput input
  print $ solve1 l

parseInput :: Parser ([Int], [Int])
parseInput = do
  _ <- string "Player 1:"
  _ <- eol
  d1 <- endBy L.decimal eol
  _ <- eol
  _ <- string "Player 2:"
  _ <- eol
  d2 <- endBy L.decimal eol
  return (d1, d2)
