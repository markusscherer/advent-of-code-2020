import Data.List
import Data.List.Split
import Data.Maybe

solve1 s b =
  let nextDepartures = zipWith (*) (map (\x -> 1 + (s `div` x)) b) b
      nextDeparture = minimum nextDepartures
      nextIndex = fromJust $ elemIndex nextDeparture nextDepartures
   in (b !! nextIndex, nextDeparture - s)

--main = do
--  input <- getContents
--  let ls = lines input
--  let start = read $ head ls :: Int
--  let busses = map read $ filter (/= "x") $ splitOn "," $ ls !! 1 :: [Int]
--  print $ solve1 start busses

tminus (a0, b0) (a1, b1) = (a0 - a1, b0 - b1)

tmul (a, b) n = (a * n, b * n)

gcd'' :: Int -> Int -> [(Int, (Int, Int))] -> (Int, Int)
gcd'' a 0 lc = snd $ fromJust $ find ((== 1) . fst) lc
gcd'' a b lc =
  let r = a `mod` b
      n = a `div` b
      lca = snd $ fromJust $ find ((== a) . fst) lc
      lcb = snd $ fromJust $ find ((== b) . fst) lc
      lcr = lca `tminus` (lcb `tmul` n)
   in gcd'' b r ((r, lcr) : lc)

gcd' a b = gcd'' a b [(a, (1, 0)), (b, (0, 1))]

lcm' [a] = a
lcm' [a, b] = lcm a b
lcm' (x : xs) = lcm x (lcm' xs)

-- see https://de.wikipedia.org/w/index.php?title=Chinesischer_Restsatz&oldid=200389318
main = do
  input <- getContents
  let ls = lines input
  let busses = map (\(x, y) -> (read x, y)) $ filter ((/= "x") . fst) $ zip (splitOn "," $ ls !! 1) [0 ..] :: [(Int, Int)]
  let moduls = map fst busses
  let as = map ((* (-1)) . snd) busses
  -- all moduls are primes
  let lcm = lcm' moduls
  let ms = map (lcm `div`) moduls
  let es = zipWith (*) (map fst $ zipWith gcd' ms moduls) ms
  let x0 = lcm + sum (zipWith (*) as es)
  print $ head $ filter (> 0) $ map (\x -> x * lcm + x0) [-100 .. 100]
