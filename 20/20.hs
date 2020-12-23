import Control.Monad
import Data.Bits
import Data.List
import Data.Maybe
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug

type Parser = Parsec Void String

type Tile = (Int, [String])

type EdgeOrientation = (Int, Int, Int, Int)

type TileWithIds = (Int, EdgeOrientation, EdgeOrientation, EdgeOrientation, EdgeOrientation)

type TileWithIdsOriented = (Int, TileWithIds)

edgeToId :: String -> Int
edgeToId s = foldr1 (.|.) $ map (bit . fst) $ filter ((== '#') . snd) $ zip [0 ..] (reverse s)

list2tuple [w, x, y, z] = (w, x, y, z)

tuple2list (w, x, y, z) = [w, x, y, z]

addId :: Tile -> TileWithIds
addId (id, t) =
  let efs = [head, map head, last, map last]
      i0 = list2tuple $ map (edgeToId . (\f -> f t)) efs
      i1 = list2tuple $ map (edgeToId . (\f -> f (map reverse t))) efs
      i2 = list2tuple $ map (edgeToId . (\f -> f (reverse t))) efs
      i3 = list2tuple $ map (edgeToId . (\f -> f (reverse (map reverse t)))) efs
   in (id, i0, i1, i2, i3)

compatible :: EdgeOrientation -> EdgeOrientation -> Bool
--compatible (an, aw, as, ae) (bn, bw, bs, be) =
compatible as bs =
  -- let l = length $ filter id [an == bs, aw == be, as == bn, ae == bw]
  let l = length $ filter id [a == b | a <- tuple2list as, b <- tuple2list bs]
   in if l == 0
        then False
        else
          if l == 1
            then True
            else error "more than one match _"

getOrientedByNeighbour :: EdgeOrientation -> TileWithIds -> Maybe TileWithIdsOriented
getOrientedByNeighbour o t@(_, e0, e1, e2, e3) =
  let m = map (compatible o) [e0, e1, e2, e3] :: [Bool]
      l = length $ filter id m
   in if l == 0
        then Nothing
        else
          if l == 1
            then Just (fromJust $ findIndex id m, t)
            else -- else error "more than one match"
              Just (fromJust $ findIndex id m, t)

readInput f = do
  input <- readFile f
  let tiles = fromJust $ parseMaybe (endBy parseTile eol) input
  return $ map addId tiles

--commutativeCombine :: (a -> a -> b) -> [a] -> [b]
--commutativeCombine f [] = []
--commutativeCombine f (x : xs) = map (f x) xs ++ commutativeCombine f xs
--

isCorner :: [TileWithIds] -> TileWithIds -> Bool
isCorner ts t@(_, o, _, _, _) =
  let other = filter ((/=) t) ts :: [TileWithIds]
   in 2 == (length $ filter isJust $ map (getOrientedByNeighbour o) other)

--findCorners :: [TileWithIds] -> [Int]
findCorners ts = map (\(i, _, _, _, _) -> i) $ filter (isCorner ts) ts

--findCorners ts = filter (isCorner ts) ts

main = do
  input <- getContents
  let tiles = fromJust $ parseMaybe (endBy parseTile eol) input
  print $ product $ findCorners $ map addId tiles

lexeme :: Parser a -> Parser a
lexeme = L.lexeme (void (some (char ' ')))

parseTile :: Parser Tile
parseTile = do
  _ <- string "Tile "
  i <- L.decimal
  _ <- char ':'
  _ <- eol
  t <- endBy (some (char '.' <|> char '#')) eol
  return (i, t)
