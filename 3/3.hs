import Control.Monad.State

hit :: [Char] -> Int -> Int
hit l i =
  let ii = i `mod` length l
      p = l !! ii
   in if p == '#' then 1 else 0

hits :: [[Char]] -> Int -> Int
hits [] i = 0
hits (t : ts) i = hit t i + hits ts (i + 3)

simulate :: [[Char]] -> (Int, Int) -> State (Int, Int, Int) Int
simulate t (dx, dy) = do
  (x, y, c) <- get
  if y >= length t
    then return c
    else do
      let tree = (t !! y) !! x
      put ((x + dx) `mod` length (head t), y + dy, c + if tree == '#' then 1 else 0)
      simulate t (dx, dy)

main = do
  contents <- getContents
  let terrain = lines contents
  print $ hits terrain 0
  print $ product $ map (\x -> evalState (simulate terrain x) (0, 0, 0)) [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
