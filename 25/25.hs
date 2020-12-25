m = 20201227

ck = 19774466

dk = 7290641

-- this could likely be faster with repeated squaring with values modulo m
expMod :: Integer -> Integer -> Integer
expMod b e = (b ^ e) `mod` m

findExponentMaybe :: Integer -> Integer -> Integer -> Maybe Integer
findExponentMaybe k m b = findExponentMaybeImpl k m b 1 0
  where
    findExponentMaybeImpl k m b v e
      | m < e = Nothing
      | otherwise = if k == v then Just e else findExponentMaybeImpl k m b ((v * b) `mod` m) (e + 1)

main = do
  print $ fmap (expMod dk) $ findExponentMaybe ck m 7
