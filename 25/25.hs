import Data.Bits

m = 20201227

ck = 19774466

dk = 7290641

expMod :: Int -> Int -> Int
expMod b e = expModImpl b e b 0 1
  where
    expModImpl :: Int -> Int -> Int -> Int -> Int -> Int
    expModImpl b e sq c v
      | e < setBit 0 c = v
      | otherwise = expModImpl b e ((sq * sq) `mod` m) (c + 1) (if testBit e c then (v * sq) `mod` m else v)

-- non-tail-recursive versions led to a stack-overflow
findExponentMaybe :: Int -> Int -> Int -> Maybe Int
findExponentMaybe k m b = findExponentMaybeImpl k m b 1 0
  where
    findExponentMaybeImpl k m b v e
      | m < e = Nothing
      | otherwise = if k == v then Just e else findExponentMaybeImpl k m b ((v * b) `mod` m) (e + 1)

main = do
  print $ expMod dk <$> findExponentMaybe ck m 7
