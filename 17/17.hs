import Data.List

type ActivePoint = (Int, Int, Int)

activeNeighbours ps p =
  let n = neighbourhood' p
   in length $ filter (`elem` n) ps

nextGeneration :: [ActivePoint] -> [ActivePoint]
nextGeneration old =
  let pa = possiblyAffectedPoints old
      new = pa \\ old
      newActive = filter (\x -> activeNeighbours old x == 3) new
      oldActive = filter (\x -> activeNeighbours old x == 2 || activeNeighbours old x == 3) old
   in oldActive ++ newActive

neighbourhood :: ActivePoint -> [ActivePoint]
neighbourhood (x, y, z) = [(x + dx, y + dy, z + dz) | dx <- [-1, 0, 1], dy <- [-1, -0, 1], dz <- [-1, 0, 1]]

neighbourhood' :: ActivePoint -> [ActivePoint]
neighbourhood' (x, y, z) = [(x + dx, y + dy, z + dz) | dx <- [-1, 0, 1], dy <- [-1, -0, 1], dz <- [-1, 0, 1], dx /= 0 || dy /= 0 || dz /= 0]

possiblyAffectedPoints :: [ActivePoint] -> [ActivePoint]
possiblyAffectedPoints ps = nub $ [n | p <- ps, n <- neighbourhood p]

main = do
  input <- getContents
  let start = [(x, y, 0) | (y, l) <- zip [0 ..] (lines input), (x, c) <- zip [0 ..] l, c == '#']
  print $ length $ (!! 6) $ iterate nextGeneration start
