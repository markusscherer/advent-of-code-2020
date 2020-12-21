findXYZ l =
  let (prs, po : pos) = splitAt 25 l
   in if po `notElem` [x + y | x <- prs, y <- prs, x /= y]
        then Just po
        else findXYZ $ tail l

findSum :: Int -> [Int] -> Int -> [Int]
findSum t l c = case compare (sum (take c l)) t of
  LT -> findSum t l (c + 1)
  GT -> findSum t (tail l) 1
  EQ -> take c l

main = do
  input <- getContents
  let numbers = map read $ lines input :: [Int]
  let (Just t) = findXYZ numbers
  let e1 = minimum $ findSum t numbers 1
  let e2 = maximum $ findSum t numbers 1
  print $ e1+e2
  print t
