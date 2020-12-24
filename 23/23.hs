import Data.List
import Data.Maybe
import Debug.Trace

findTarget :: Int -> Int -> Int -> [Int] -> Int
findTarget t inf max [] = if inf /= 0 then inf else max
findTarget t inf max (x : xs)
  | x == (t -1) = x
  | x < t && x > inf = findTarget t x max xs
  | x > t && x > max = findTarget t inf x xs
  | otherwise = findTarget t inf max xs

shuffleRound 0 l = l
shuffleRound r (c : r1 : r2 : r3 : rest) =
  ( if r `mod` 10 == 0
      then trace (show r)
      else id
  )
    $ let target = findTarget c 0 0 rest
          (p1, p2) = splitAt (1 + (fromJust $ findIndex (== target) rest)) rest
       in shuffleRound (r -1) (p1 ++ [r1, r2, r3] ++ p2 ++ [c])

solve1 = tail $ head $ take 1 $dropWhile ((/= 1) . head) $ iterate rotate $ shuffleRound 100 [3, 8, 9, 5, 4, 7, 6, 1, 2]

solve2 =
  let res = shuffleRound 10000000 ([3, 8, 9, 1, 2, 5, 4, 6, 7] ++ [10 .. 1000000])
      idx = fromJust $ findIndex (== 1) res
   in (idx, res !! (idx + 1), res !! (idx + 2))

rotate (x : xs) = xs ++ [x]

main = putStrLn $ show solve1
