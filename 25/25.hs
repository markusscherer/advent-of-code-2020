m = 20201227

--m = 113

f b e = (b ^ e) `mod` m

ff b 0 = [1]
ff b e =
  let xs@(x : _) = ff b (e -1)
   in ((b * x) `mod` m) : xs

ffIO b 0 = do
  print 1
  return 1
ffIO b e = do
  x <- ffIO b (e -1)
  let res = (b * x) `mod` m
  print res
  return res

--f b 0 = 1
--f b e = (b * f b (e -1)) `mod` m

--ck = 5764801

-- dk = 17807724
--
nubSorted [] = []
nubSorted [x] = [x]
nubSorted (x : y : xs)
  | x == y = nubSorted (y : xs)
  | otherwise = x : nubSorted (y : xs)

ck = 19774466

fck = [2, 31, 197, 1619]

dk = 7290641

findExponent k = head $ dropWhile ((/= k) . fst) $ map (\x -> (f 7 x, x)) [1 .. (20201227 * 2)]

--main = do
--  let (_, ce) = findExponent ck
--  --      (_, de) = findExponent dk
--  print (f dk ce)
main = do
  ffIO 7 (m * 2)
