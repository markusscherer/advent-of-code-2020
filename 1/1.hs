
data Tree = Leaf | Node Int Tree Tree

instance Show Tree where
  show Leaf = "()"
  show (Node i l r) = "(" ++ show i ++ show l ++ show r ++ ")"

insert :: Int -> Tree -> Tree
insert i Leaf = Node i Leaf Leaf
insert i (Node a l r) = if a < i then Node a (insert i l) r else Node a l (insert i r)


main = do
  contents <- getContents
  let contents' = lines contents
  let numbers = map read contents' :: [Int]
  let t = foldr insert Leaf numbers
  let s = head $ filter (\(x,y,z) -> x+y+z == 2020) [(i,j,k) | i <- numbers, j <- numbers, k <- numbers]


  putStr $ show $ (\(x,y,z) -> x*y*z) $ s
--   args <- getArgs
--   content <- readFile (args !! 0)
--   let l = lines content
--   putStr l
