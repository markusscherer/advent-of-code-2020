import Data.List
import Data.Maybe
import qualified Data.Set as S
import Data.Void
import Debug.Trace
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

solve1 l = getScore $ play l

getScore l = sum $ zipWith (*) [1 ..] (reverse l)

p1won (_, []) = True
p1won ([], _) = False

getWinnerStack (s, []) = s
getWinnerStack ([], s) = s

--  | s `elem` already = playRec already ((xs ++ [x, y]), ys)

playRec :: S.Set ([Int], [Int]) -> ([Int], [Int]) -> ([Int], [Int])
playRec already s@(_, []) = s
playRec already s@([], _) = s
playRec already s@((x : xs), (y : ys))
  | s `elem` already = ((xs ++ [x, y]), [])
  | x <= length xs && y <= length ys =
    let res = playRec (S.empty) (take x xs, take y ys)
     in if p1won res
          then playRec (S.insert s already) ((xs ++ [x, y]), ys)
          else playRec (S.insert s already) (xs, (ys ++ [y, x]))
  | x > y = playRec (S.insert s already) ((xs ++ [x, y]), ys)
  | y > x = playRec (S.insert s already) (xs, (ys ++ [y, x]))

solve2 l = getScore $ getWinnerStack $ playRec S.empty l

main = do
  input <- getContents
  let l = fromJust $ parseMaybe parseInput input
  print $ solve1 l
  print $ solve2 l

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
