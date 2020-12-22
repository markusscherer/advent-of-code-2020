import Data.List

type ActivePoint = (Int, Int, Int)

type ActivePoint' = (Int, Int, Int, Int)

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

activeNeighbours4 ps p =
  let n = neighbourhood4' p
   in length $ filter (`elem` n) ps

nextGeneration4 :: [ActivePoint'] -> [ActivePoint']
nextGeneration4 old =
  let pa = possiblyAffectedPoints4 old
      new = pa \\ old
      newActive = filter (\x -> activeNeighbours4 old x == 3) new
      oldActive = filter (\x -> activeNeighbours4 old x == 2 || activeNeighbours4 old x == 3) old
   in oldActive ++ newActive

neighbourhood4 :: ActivePoint' -> [ActivePoint']
neighbourhood4 (x, y, z, w) = [(x + dx, y + dy, z + dz, w + dw) | dx <- [-1, 0, 1], dy <- [-1, -0, 1], dz <- [-1, 0, 1], dw <- [-1, 0, 1]]

neighbourhood4' :: ActivePoint' -> [ActivePoint']
neighbourhood4' (x, y, z, w) = [(x + dx, y + dy, z + dz, w + dw) | dx <- [-1, 0, 1], dy <- [-1, -0, 1], dz <- [-1, 0, 1], dw <- [-1, 0, 1], dx /= 0 || dy /= 0 || dz /= 0 || dw /= 0]

possiblyAffectedPoints4 :: [ActivePoint'] -> [ActivePoint']
possiblyAffectedPoints4 ps = nub $ [n | p <- ps, n <- neighbourhood4 p]

main = do
  input <- getContents
  let start = [(x, y, 0) | (y, l) <- zip [0 ..] (lines input), (x, c) <- zip [0 ..] l, c == '#']
  print $ length $ (!! 6) $ iterate nextGeneration start
  let start4 = [(x, y, 0, 0) | (y, l) <- zip [0 ..] (lines input), (x, c) <- zip [0 ..] l, c == '#']
  print $ length $ (!! 6) $ iterate nextGeneration4 start4
