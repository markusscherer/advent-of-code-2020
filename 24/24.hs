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

data Direction = E | SE | SW | W | NW | NE deriving (Show, Eq, Enum)

data Color = White | Black

go E (x, y) = (x + 1, y)
go SE (x, y) = (x + 1, y -1)
go SW (x, y) = (x, y -1)
go W (x, y) = (x -1, y)
go NW (x, y) = (x -1, y + 1)
go NE (x, y) = (x, y + 1)

getTarget = foldr go (0, 0)

toggle Nothing = Just Black
toggle (Just Black) = Nothing

toggle' m c =
  let bn = length $ filter isJust $ getNeighbourhood m c
      b = isJust $ M.lookup c m
   in if b
        then if bn == 0 || bn > 2 then Nothing else Just (c, Black)
        else if bn == 2 then Just (c, Black) else Nothing

getNeighbourhood m x = map ((`M.lookup` m) . (\c -> go c x)) [E .. NE]

allRelevantTiles :: M.Map (Int, Int) Color -> [(Int, Int)]
allRelevantTiles m = nub $ concatMap (\c -> map (\d -> go d c) [E .. NE]) $ M.keys m

solve1 ds = length $ foldr (M.alter toggle . getTarget) M.empty ds

steps :: Int -> M.Map (Int, Int) Color -> M.Map (Int, Int) Color
steps 0 m = m
steps n m =
  let m' = M.fromList $ mapMaybe (toggle' m) $ allRelevantTiles m
   in steps (n -1) m'

solve2 ds =
  let start = foldr (M.alter toggle . getTarget) M.empty ds
   in M.size $ steps 100 start

main = do
  input <- getContents
  let ds = fromJust $ parseMaybe (endBy parseTrace eol) input
  print $ solve1 ds
  print $ solve2 ds

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
