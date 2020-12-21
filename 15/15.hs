import Data.List
import Data.List.Split
import qualified Data.Map as M

play :: [(Int, Int)] -> [(Int, Int)]
play (x@(tx, vx) : xs) =
  let o = find ((== vx) . snd) xs
      f Nothing = 0
      f (Just (to, vo)) = tx - to
   in (tx + 1, f o) : x : xs

solve x xs =
  head $
    head $
      dropWhile ((/= x) . fst . head) $
        iterate play (reverse $ zip [1 ..] xs)

solve1 = solve 2020

solve2 xs =
  let start = init $ zip xs [1 ..]
      (vlast, turn) = last $ zip xs [1 ..]
   in fst $ play' (M.fromList start) vlast turn 30000000

play' :: M.Map Int Int -> Int -> Int -> Int -> (Int, M.Map Int Int)
play' m last turn till
  | turn == till = (last, m)
  | otherwise = case M.lookup last m of
    Nothing -> play' (M.insert last turn m) 0 (turn + 1) till
    (Just x) -> play' (M.insert last turn m) (turn - x) (turn + 1) till

main = do
  numbers <- map read `fmap` splitOn "," `fmap` getContents :: IO [Int]
  print $ solve1 numbers
  print $ solve2 numbers
