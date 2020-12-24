import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Void
import Debug.Trace
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug

type Parser = Parsec Void String

data Direction = E | SE | SW | W | NW | NE deriving (Show, Eq)

data Color = White | Black

go E (x, y) = (x + 1, y)
go SE (x, y) = (x + 1, y -1)
go SW (x, y) = (x, y -1)
go W (x, y) = (x -1, y)
go NW (x, y) = (x -1, y + 1)
go NE (x, y) = (x, y + 1)

getTarget ds = foldr go (0, 0) ds

toggle Nothing = Just Black
toggle (Just White) = Just Black
toggle (Just Black) = Nothing

solve1 ds = length $ foldr (M.alter toggle) M.empty $ map getTarget ds

main = do
  input <- getContents
  let ds = fromJust $ parseMaybe (endBy parseTrace eol) input
  print $ solve1 ds

parseTrace :: Parser [Direction]
parseTrace = some (choice [parseE, parseSE, parseSW, parseW, parseNW, parseNE])

parseE :: Parser Direction
parseE = string "e" >> return E

parseSE :: Parser Direction
parseSE = string "se" >> return SE

parseSW :: Parser Direction
parseSW = string "sw" >> return SW

parseW :: Parser Direction
parseW = string "w" >> return W

parseNE :: Parser Direction
parseNE = string "ne" >> return NE

parseNW :: Parser Direction
parseNW = string "nw" >> return NW
