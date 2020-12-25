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

data Orientation = O0 | O90 | O180 | O270 | M0 | M90 | M180 | M270 deriving (Show, Eq, Enum, Ord)

type EdgeOrientation = (Int, Int, Int, Int)

type TileWithEdges = (Int, Orientation, EdgeOrientation)

edgeToId :: String -> Int
edgeToId s = foldr ((.|.) . bit . fst) 0 $ filter ((== '#') . snd) $ zip [0 ..] (reverse s)

list2tuple [w, x, y, z] = (w, x, y, z)

rotateTile :: [String] -> [String]
rotateTile = reverse . transpose

addId :: Tile -> [TileWithEdges]
addId (id, t) =
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

isNeighbour :: TileWithEdges -> TileWithEdges -> Bool
isNeighbour (id1, o1, e1) (id2, o2, e2) = compatible' e1 e2

isEastNeighbour :: TileWithEdges -> TileWithEdges -> Bool
isEastNeighbour (id1, _, e1) (id2, _, e2) = id1 /= id2 && compatibleEast e1 e2

isSouthNeighbour :: TileWithEdges -> TileWithEdges -> Bool
isSouthNeighbour (id1, _, e1) (id2, _, e2) = id1 /= id2 && compatibleSouth e1 e2

eastNeighbours :: [TileWithEdges] -> TileWithEdges -> [TileWithEdges]
eastNeighbours = neighbours isEastNeighbour

southNeighbours :: [TileWithEdges] -> TileWithEdges -> [TileWithEdges]
southNeighbours = neighbours isSouthNeighbour

neighbours :: (TileWithEdges -> TileWithEdges -> Bool) -> [TileWithEdges] -> TileWithEdges -> [TileWithEdges]
neighbours pred ts t =
  let en = filter (pred t) ts
   in if null en
        then [t]
        else
          if length en == 1
            then t : neighbours pred ts (head en)
            else error "neighbouring tile was not unique!"

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

isCorner :: [TileWithEdges] -> TileWithEdges -> Bool
isCorner ts t@(i, o, e) =
  let other = filter ((/= i) . fst3) ts :: [TileWithEdges]
   in 2 == length (filter (isNeighbour t) other)

findCorners ts = filter (isCorner ts) ts

monster =
  [ "                  # ",
    "#    ##    ##    ###",
    " #  #  #  #  #  #   "
  ]

addTuple (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

monsterPattern = [(x, y) | (y, l) <- zip [0 ..] monster, (x, c) <- zip [0 ..] l, c == '#']

arrayToString a =
  let (_, (w, h)) = A.bounds a
   in unlines [[a A.! (x, y) | x <- [0 .. (w -1)]] | y <- [0 .. (h -1)]]

hasMonster a i = all ((== '#') . (a A.!) . addTuple i) monsterPattern

highlightMonster i a =
  if hasMonster a i
    then a A.// [(i, 'O') | i <- map (addTuple i) monsterPattern]
    else a

tileWidth = length . head

tileHeight = length

solve2 :: [String] -> Int
solve2 t =
  let w = tileWidth t
      h = tileHeight t
      a = A.array ((0, 0), (w, h)) [((x, y), c) | (y, l) <- zip [0 ..] t, (x, c) <- zip [0 ..] l]
   in length $ filter (== '#') $ arrayToString $ foldr highlightMonster a [(x, y) | x <- [0 .. w - tileWidth monster], y <- [0 .. h - tileHeight monster]]

solve1 :: [[TileWithEdges]] -> Int
solve1 ts =
  let northEdge = head ts
      southEdge = last ts
   in product $ map fst3 [head northEdge, last northEdge, head southEdge, last southEdge]

cropEdges :: [String] -> [String]
cropEdges = init . tail . map (init . tail)

arrangePicture :: M.Map Int [String] -> [[TileWithEdges]] -> [String]
arrangePicture tileMap =
  concatMap (map concat . transpose . map (cropEdges . (\(i, o, _) -> applyOrientation o $ fromJust $ M.lookup i tileMap)))

arrangeFromNorthWest ts nw = map (eastNeighbours ts) $ southNeighbours ts nw

main = do
  input <- getContents
  let tiles = fromJust $ parseMaybe (endBy parseTile eol) input
  let tileMap = M.fromList tiles
  let tilesWithEdges = concatMap addId tiles
  let potentialCorners = findCorners tilesWithEdges
  let arrangedTiles = head $ dropWhile ((/= length tiles) . (sum . map length)) $ map (arrangeFromNorthWest tilesWithEdges) potentialCorners
  let arrangedPicture = arrangePicture tileMap arrangedTiles
  print $ solve1 arrangedTiles
  print $ minimum $ map (\o -> (solve2 $ applyOrientation o arrangedPicture, o)) [O0 .. M270]

parseTile :: Parser Tile
parseTile = do
  _ <- string "Tile "
  i <- L.decimal
  _ <- char ':'
  _ <- eol
  t <- endBy (some (char '.' <|> char '#')) eol
  return (i, t)
