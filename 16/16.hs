import Data.Maybe
import Data.Void
import Data.Boolean.SatSolver
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Interval = (Int, Int)

type UnionInterval = (Interval, Interval)

type Category = (String, UnionInterval)

type Ticket = [Int]

type Parser = Parsec Void String

inInterval :: Interval -> Int -> Bool
inInterval (l, u) i = l <= i && i <= u

inUnionInterval :: UnionInterval -> Int -> Bool
inUnionInterval (a, b) i = inInterval a i || inInterval b i

solve1 :: [Category] -> [Ticket] -> Int
solve1 cs ts =
  let uis = map snd cs
      f i = not $ any (\ui -> inUnionInterval ui i) uis
   in sum $ filter f $ concat ts

main = do
  input <- getContents
  let (categories, ticket, nearby) = fromJust $ parseMaybe parseInput input
  print $ solve1 categories nearby

parseInterval :: Parser Interval
parseInterval = do
  l <- L.decimal
  _ <- char '-'
  u <- L.decimal
  return (l, u)

parseUnionInterval :: Parser UnionInterval
parseUnionInterval = do
  a <- parseInterval
  _ <- string " or "
  b <- parseInterval
  return (a, b)

parseCategory :: Parser Category
parseCategory = do
  name <- some (alphaNumChar <|> char ' ')
  _ <- string ": "
  ui <- parseUnionInterval
  return (name, ui)

parseTicket :: Parser Ticket
parseTicket = sepBy L.decimal (char ',')

parseInput :: Parser ([Category], Ticket, [Ticket])
parseInput = do
  categories <- endBy parseCategory eol
  _ <- eol
  _ <- string "your ticket:"
  _ <- eol
  ticket <- parseTicket
  _ <- eol
  _ <- eol
  _ <- string "nearby tickets:"
  nearbyTickets <- endBy parseTicket eol
  return (categories, ticket, nearbyTickets)
