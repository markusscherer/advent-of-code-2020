locate :: Int -> Int -> Char -> Char -> String -> Int
locate lo up _ _ [] = lo
locate lo up lc uc (x : xs)
  | x == uc =
    let s = ((up + 1) - lo) `div` 2
     in locate (lo + s) up lc uc xs
  | x == lc =
    let s = ((up + 1) - lo) `div` 2
     in locate lo (up - s) lc uc xs

locateBF = locate 0 127 'F' 'B'

locateLR = locate 0 7 'L' 'R'

seatToId :: (Int, Int) -> Int
seatToId (row, col) = row * 8 + col

allSeats :: [(Int, Int)]
allSeats = do
  row <- [1 .. 105]
  col <- [0 .. 7]
  return (row, col)

main = do
  input <- getContents
  let l = lines input
  let scanned = map ((\(bf, lr) -> (locateBF bf, locateLR lr)) . (splitAt 7)) l
  --  print $ maximum $ map (\x -> (seatToId x, x)) scanned
  print $ map seatToId $ filter (not . (`elem` scanned)) allSeats
