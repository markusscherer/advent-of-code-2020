import Data.List
import Data.Maybe
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug

type Parser = Parsec Void String

solve1 :: [([String], [String])] -> Int
solve1 l =
  let ingredients = nub $ concatMap fst l
      allergens = nub $ concatMap snd l
      possibleBad = map (\a -> (a, foldr1 intersect $ map fst $ filter (\x -> a `elem` (snd x)) l)) allergens
      impossibleBad = ingredients \\ (concatMap snd possibleBad)
   in sum $ map (length . (filter (`elem` impossibleBad)) . fst) l

fixBadIngredients :: [(String, [String])] -> [(String, String)] -> [(String, String)]
fixBadIngredients [] fixed = fixed
fixBadIngredients l fixed =
  let (newFixed, x) = partition ((== 1) . length . snd) l
      newFixed' = map (\(i, a) -> (i, head a)) newFixed
      newFixedIngredients = map snd newFixed'
      cleaned = map (\(i, as) -> (i, as \\ newFixedIngredients)) x
   in fixBadIngredients cleaned (fixed ++ newFixed')

solve2 :: [([String], [String])] -> String
solve2 l =
  let ingredients = nub $ concatMap fst l
      allergens = nub $ concatMap snd l
      possibleBad = map (\a -> (a, foldr1 intersect $ map fst $ filter (\x -> a `elem` (snd x)) l)) allergens
   in intercalate "," $ map snd $ sort $ fixBadIngredients possibleBad []

main = do
  input <- getContents
  let l = fromJust $ parseMaybe (endBy parseLine eol) input
  print $ solve1 l
  print $ solve2 l

parseWord :: Parser String
parseWord = some lowerChar

parseLine :: Parser ([String], [String])
parseLine = do
  ingredients <- endBy parseWord (char ' ')
  _ <- string "(contains "
  allergens <- sepBy parseWord (string ", ")
  _ <- char ')'
  return (ingredients, allergens)
