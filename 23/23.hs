import Control.Monad.Fix
import Control.Monad.ST
import Data.Array.Base
import Data.Array.IArray
import Data.Array.ST

cups :: [Int]
-- cups = [3, 8, 9, 1, 2, 5, 4, 6, 7]
cups = [3, 8, 9, 5, 4, 7, 6, 1, 2]

toNextArray :: [Int] -> ST s (STUArray s Int Int)
toNextArray xs = do
  let l = length xs
  a <- newArray (1, l) 0
  mapM_
    (uncurry (writeArray a))
    (zip xs (rotate xs))
  return a

fromNextArray :: UArray Int Int -> Int -> [Int]
fromNextArray a start = let l = rangeSize $ bounds a in fromNextArrayImpl (elems a) start l
  where
    fromNextArrayImpl _ _ 0 = []
    fromNextArrayImpl xs cur c = cur : fromNextArrayImpl xs (xs !! (cur - 1)) (c - 1)

wrapAround :: Int -> Int -> Int
wrapAround m x = ((((x - 1) `mod` m) + m) `mod` m) + 1

shuffleRound :: STUArray s Int Int -> Int -> Int -> ST s (STUArray s Int Int)
shuffleRound a 0 _ = return a
shuffleRound a r i = do
  n1 <- readArray a i
  n2 <- readArray a n1
  n3 <- readArray a n2
  n4 <- readArray a n3
  let pickup = [n1, n2, n3]
  (_, m) <- getBounds a
  let ft =
        fix
          ( \rec x ->
              let w = wrapAround m (x -1)
               in if w `elem` pickup then rec w else w
          )
  let target = ft i
  nn <- readArray a target
  writeArray a i n4
  writeArray a target n1
  writeArray a n3 nn
  shuffleRound a (r -1) n4

shuffleRounds :: [Int] -> Int -> ST s (STUArray s Int Int)
shuffleRounds xs r =
  do
    next <- toNextArray xs
    shuffleRound next r (head xs)

solve1 :: [Int]
solve1 = tail $ fromNextArray (runSTUArray $ shuffleRounds cups 100) 1

solve2 :: Int
solve2 =
  let r = 10000000
      m = 1000000
      shuffled = runSTUArray $ shuffleRounds (cups ++ [10 .. m]) r
      n1 = shuffled ! 1
      n2 = shuffled ! n1
   in n1 * n2

rotate :: [a] -> [a]
rotate [] = []
rotate (x : xs) = xs ++ [x]

main :: IO ()
main = do
  print solve1
  print solve2
