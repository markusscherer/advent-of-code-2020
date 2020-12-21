import Data.Array
import Data.List
import Data.Maybe

type Seat = Char

type Neighborhood = [Seat]

type Coordinates = (Int, Int)

type Map = Array Coordinates Char

type NeighborhoodFunction = Map -> Coordinates -> Neighborhood

type ModificationFunction = Seat -> Neighborhood -> Seat

toArray :: [String] -> Map
toArray l =
  let h = length l
      w = length $ head l
   in array
        ((0, 0), (h -1, w -1))
        [((y, x), c) | (y, ll) <- zip [0 ..] l, (x, c) <- zip [0 ..] ll]

takeWhileChange :: (Eq a) => [a] -> [a]
takeWhileChange [] = []
takeWhileChange [a] = [a]
takeWhileChange (a : b : xs) = if a == b then [a] else a : takeWhileChange (b : xs)

neighbourhood :: NeighborhoodFunction
neighbourhood a (x, y) =
  let ((minx, miny), (maxx, maxy)) = bounds a
      raw = [(x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1]]
   in map (a !) $ filter (\(x', y') -> minx <= x' && x' <= maxx && miny <= y' && y' <= maxy && (x', y') /= (x, y)) raw

modifySeat :: ModificationFunction
modifySeat 'L' n | '#' `notElem` n = '#'
modifySeat '#' n | length (filter (== '#') n) >= 4 = 'L'
modifySeat c _ = c

modifyArray :: NeighborhoodFunction -> ModificationFunction -> Map -> Map
modifyArray n m a =
  let b = bounds a
   in array b [(i, m (a ! i) (n a i)) | i <- indices a]

addTuple (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

neighbourhoodBySight :: NeighborhoodFunction
neighbourhoodBySight a (x, y) =
  let ((minx, miny), (maxx, maxy)) = bounds a
      dirs = [(dx, dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], dx /= 0 || dy /= 0]
      check (x', y') = minx <= x' && x' <= maxx && miny <= y' && y' <= maxy && (x', y') /= (x, y)
      firstSeen =
        map
          ( \d ->
              safeHead $
                filter (/= '.') $
                  map (a !) $ takeWhile check $ drop 1 $ iterate (addTuple d) (x, y)
          )
          dirs
   in catMaybes firstSeen

modifySeat' :: ModificationFunction
modifySeat' 'L' n | '#' `notElem` n = '#'
modifySeat' '#' n | length (filter (== '#') n) >= 5 = 'L'
modifySeat' c _ = c

main =
  do
    input <- getContents
    let f = lines input
    let stable1 = last $ takeWhileChange $ iterate (modifyArray neighbourhood modifySeat) (toArray f)
    let stable2 = last $ takeWhileChange $ iterate (modifyArray neighbourhoodBySight modifySeat') (toArray f)
    print $ length $ filter (== '#') $ elems stable1
    print $ length $ filter (== '#') $ elems stable2
