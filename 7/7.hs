import Data.List
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

type Bag = String

type Container = (Bag, [(Int, Bag)])

isContainer :: Container -> Bag -> Bool
isContainer (ob, c) b = any ((== b) . snd) c

transitiveContainers :: [Container] -> [Bag] -> [Bag]
transitiveContainers cs bs =
  let nn = map fst $ filter (\c -> any (isContainer c) bs) cs
      bs' = nub $ bs ++ nn
   in if length bs' == length bs then bs else transitiveContainers cs bs'

countContainers :: [Container] -> Bag -> Int
countContainers cs b =
  let (Just (_, bs')) = find ((== b) . fst) cs
   in 1 + sum (map (\(count, bag) -> count * countContainers cs bag) bs')

main = do
  input <- getContents
  let (Just containers) = parseMaybe (many parseLine) input
  --putStr $ show $ (\x -> x - 1) $ length $ transitiveContainers containers ["shiny gold"]
  print $ (\x -> x - 1) $countContainers containers "shiny gold"

lower :: Parser Char
lower = choice $ map char "abcdefghijklmnopqrstuvwxyz"

parseContain :: Parser (Int, Bag)
parseContain = do
  c <- L.decimal
  _ <- char ' '
  modifier <- many lower
  _ <- char ' '
  color <- many lower
  _ <- choice [string " bags", string " bag"]
  return (c, modifier ++ " " ++ color)

parseNoContain :: Parser [(Int, Bag)]
parseNoContain = do
  _ <- string "no other bags"
  return []

parseLine :: Parser Container
parseLine = do
  modifier <- many lower
  _ <- char ' '
  color <- many lower
  _ <- string " bags contain "
  l <- try parseNoContain <|> sepBy parseContain (string ", ")
  _ <- char '.'
  _ <- eol
  return (modifier ++ " " ++ color, l)
