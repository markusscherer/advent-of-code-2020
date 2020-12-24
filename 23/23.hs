import Data.List
import Data.Maybe

shuffleRound 0 l = l
shuffleRound r (c : r1 : r2 : r3 : rest) =
  let smallerRest = filter (< c) rest
      target = if null smallerRest then maximum rest else maximum smallerRest
      (p1, p2) = splitAt (1 + (fromJust $ findIndex (== target) rest)) rest
   in shuffleRound (r -1) (p1 ++ [r1, r2, r3] ++ p2 ++ [c])

solve1 = tail $ head $ take 1 $dropWhile ((/= 1) . head) $ iterate rotate $ shuffleRound 100 [3, 8, 9, 5, 4, 7, 6, 1, 2]

rotate (x : xs) = xs ++ [x]

main = putStrLn $ concatMap show solve1
