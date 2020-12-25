main = do
  contents <- getContents
  let numbers = map read (lines contents) :: [Int]
  print $ head [i * j | i <- numbers, j <- numbers, i + j == 2020]
  print $ head [i * j * k | i <- numbers, j <- numbers, k <- numbers, i + j + k == 2020]
