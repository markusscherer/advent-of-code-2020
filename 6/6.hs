import Data.List

groupBlocks :: [String] -> [String] -> [[String]]
groupBlocks [] cu = [reverse cu]
groupBlocks ([] : ss) cu = reverse cu : groupBlocks ss []
groupBlocks (s : ss) cu = groupBlocks ss (s : cu)

countForGroup :: [String] -> Int
countForGroup g = (length . nub) (concat g)

countForGroup2 :: [String] -> Int
countForGroup2 g = length $ foldr intersect ['a' .. 'z'] g

main = do
  input <- getContents
  let l = lines input
  let g = groupBlocks l []
  print $ sum $ map countForGroup2 g
