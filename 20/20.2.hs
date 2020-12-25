import Control.Monad
import qualified Data.Array as A
import Data.Bits
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug

type Parser = Parsec Void String

type Tile = (Int, [String])

data Orientation = O0 | O90 | O180 | O270 | M0 | M90 | M180 | M270 deriving (Show, Eq, Enum)

type EdgeOrientation = (Int, Int, Int, Int)

type TileWithIds = (Int, EdgeOrientation, EdgeOrientation, EdgeOrientation, EdgeOrientation)

type TileWithIds' = (Int, Orientation, EdgeOrientation)

type TileWithIdsOriented = (Int, TileWithIds)

edgeToId :: String -> Int
edgeToId s = foldr (.|.) 0 $ map (bit . fst) $ filter ((== '#') . snd) $ zip [0 ..] (reverse s)

list2tuple [w, x, y, z] = (w, x, y, z)

tuple2list (w, x, y, z) = [w, x, y, z]

rotateTile = reverse . transpose

addId' :: Tile -> [TileWithIds']
addId' (id, t) =
  let efs = [head, map head, last, map last]
      l1 = map (\to -> map (edgeToId . (\f -> f to)) efs) (take 4 $ iterate rotateTile t)
      l2 = map (\to -> map (edgeToId . (\f -> f to)) efs) (take 4 $ iterate rotateTile (reverse t))
   in zipWith (\o e -> (id, o, list2tuple e)) [O0 .. M270] (l1 ++ l2)

compatible' :: EdgeOrientation -> EdgeOrientation -> Bool
compatible' (an, aw, as, ae) (bn, bw, bs, be) = or [an == bs, aw == be, as == bn, ae == bw]

compatibleSouth :: EdgeOrientation -> EdgeOrientation -> Bool
compatibleSouth (_, _, as, _) (bn, _, _, _) = as == bn

compatibleEast :: EdgeOrientation -> EdgeOrientation -> Bool
compatibleEast (_, _, _, ae) (_, bw, _, _) = ae == bw

isNeighbour :: TileWithIds' -> TileWithIds' -> Bool
isNeighbour (id1, o1, e1) (id2, o2, e2) = compatible' e1 e2

isEastNeighbour :: TileWithIds' -> TileWithIds' -> Bool
isEastNeighbour (id1, _, e1) (id2, _, e2) = id1 /= id2 && compatibleEast e1 e2

isSouthNeighbour :: TileWithIds' -> TileWithIds' -> Bool
isSouthNeighbour (id1, _, e1) (id2, _, e2) = id1 /= id2 && compatibleSouth e1 e2

eastNeighbours = neighbours isEastNeighbour

southNeighbours = neighbours isSouthNeighbour

neighbours pred ts t =
  let en = (filter (pred t) ts)
   in if null en
        then [[t]]
        else [t] : neighbours pred ts (head en)

readInput f = do
  input <- readFile f
  let tiles = fromJust $ parseMaybe (endBy parseTile eol) input
  return $ concatMap addId' tiles

applyOrientation :: Orientation -> [String] -> [String]
applyOrientation O0 t = t
applyOrientation O90 t = rotateTile t
applyOrientation O180 t = rotateTile $ rotateTile t
applyOrientation O270 t = rotateTile $ rotateTile $ rotateTile t
applyOrientation M0 t = reverse t
applyOrientation M90 t = applyOrientation O90 $ reverse t
applyOrientation M180 t = applyOrientation O180 $ reverse t
applyOrientation M270 t = applyOrientation O270 $ reverse t

fst3 (a, b, c) = a

isCorner :: [TileWithIds'] -> TileWithIds' -> Bool
isCorner ts t@(i, o, e) =
  let other = filter ((/= i) . fst3) ts :: [TileWithIds']
   in 2 == (length $ filter (isNeighbour t) other)

getId (i, _, _, _, _) = i

findCorners ts = filter (isCorner ts) ts

monster =
  [ "                  # ",
    "#    ##    ##    ###",
    " #  #  #  #  #  #   "
  ]

addTuple (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

monsterPattern = [(x, y) | (y, l) <- zip [0 ..] monster, (x, c) <- zip [0 ..] l, c == '#']

arrayToString a = unlines $ map (map (a A.!)) [[(x, y) | x <- [0 .. 95]] | y <- [0 .. 95]]

hasMonster a i = all (== '#') $ map (a A.!) $ map (addTuple i) monsterPattern

highlightMonster i a =
  if hasMonster a i
    then a A.// [(i, 'O') | i <- map (addTuple i) monsterPattern]
    else a

--solve2 :: [String] -> Int
solve2 t =
  let a = A.array ((0, 0), (95, 95)) [((x, y), c) | (y, l) <- zip [0 ..] t, (x, c) <- zip [0 ..] l]
   in show $ length $ filter (=='#') $ arrayToString $ foldr highlightMonster a [(x, y) | x <- [0 .. 75], y <- [0 .. 92]]

main = do
  input <- getContents
  let tiles = fromJust $ parseMaybe (endBy parseTile eol) input
  let tileMap = M.fromList tiles
  let ts = concatMap addId' tiles
  let nw = (!! 1) $ findCorners ts -- this potential start corner was found empirically
  -- print $
  putStrLn $
    --    unlines $
    solve2 $
      (rotateTile . rotateTile) $ -- this rotation was found empirically
        concat $
          map (map (intercalate "")) $
            map (init . tail) $
              map (map (map (init . tail))) $
                (map transpose) $
                  map (map ((\(i, o, _) -> applyOrientation o $ fromJust $ M.lookup i tileMap) . head)) $
                    map (eastNeighbours ts . head) $ southNeighbours ts nw

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
