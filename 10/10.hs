import Control.Arrow ((***))
import Control.Monad (join)
import Control.Monad.Memo
import Data.List

mapTuple = join (***)

diffs :: [Int] -> [Int]
diffs [x, y] = [y - x]
diffs (x : (y : zs)) = (y - x) : diffs (y : zs)

variants :: [Int] -> [Int] -> [[Int]]
variants rev [] = [reverse rev]
variants rev [a] = [reverse $ a : rev]
variants rev@(r : _) (x : y : xs) =
  let diff = y - r
   in if diff > 3
        then variants (x : rev) (y : xs)
        else variants (x : rev) (y : xs) ++ variants rev (y : xs)
variants [] (x : y : xs) = variants [x] (y : xs)

countVariants :: [Int] -> Memo [Int] Int Int
countVariants [x, y, z]
  | y - x > 3 = return 0
  | z - x > 3 = return 1
  | otherwise = error "last element not 3 bigger than second to last"
countVariants (x : y : z : xs)
  | y - x > 3 = return 0
  | otherwise = do
    n1 <- memo countVariants (y : z : xs)
    n2 <- memo countVariants (x : z : xs)
    return (n1 + n2)

main = do
  input <- getContents
  let rawNumbers = map read $ lines input :: [Int]
  let m = maximum rawNumbers
  let numbers = sort $ (m + 3) : 0 : rawNumbers
  print $ partition (== 1) $ diffs numbers
  print $ uncurry (*) $ mapTuple length $ partition (== 1) $ diffs numbers
  let adapters = numbers
  --  putStrLn $ intercalate "\n" $ map show $ variants [] adapters
  print (startEvalMemo (countVariants adapters) :: Int)
