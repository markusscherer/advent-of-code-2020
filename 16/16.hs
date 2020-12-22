import Data.Boolean.SatSolver
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Interval = (Int, Int)

type UnionInterval = (Interval, Interval)

type Category = (String, UnionInterval)

type Ticket = [Int]

type Parser = Parsec Void String

c = ("asdf", ((1, 2), (4, 5)))

d = ("fdsa", ((2, 4), (6, 7)))

vm = buildVarMap [c, d]

vm' :: VarMap
vm' = M.fromList [((0, ("class", ((0, 1), (4, 19)))), Var 0), ((0, ("row", ((0, 5), (8, 19)))), Var 1), ((0, ("seat", ((0, 13), (16, 19)))), Var 2), ((1, ("class", ((0, 1), (4, 19)))), Var 3), ((1, ("row", ((0, 5), (8, 19)))), Var 4), ((1, ("seat", ((0, 13), (16, 19)))), Var 5), ((2, ("class", ((0, 1), (4, 19)))), Var 6), ((2, ("row", ((0, 5), (8, 19)))), Var 7), ((2, ("seat", ((0, 13), (16, 19)))), Var 8)]

inInterval :: Interval -> Int -> Bool
inInterval (l, u) i = l <= i && i <= u

inUnionInterval :: UnionInterval -> Int -> Bool
inUnionInterval (a, b) i = inInterval a i || inInterval b i

invalidNumber :: [UnionInterval] -> Int -> Bool
invalidNumber uis i = not $ any (\ui -> inUnionInterval ui i) uis

invalid :: [UnionInterval] -> Ticket -> Bool
invalid uis t = any (invalidNumber uis) t

solve1 :: [Category] -> [Ticket] -> Int
solve1 cs ts =
  let uis = map snd cs
   in sum $ filter (invalidNumber uis) $ concat ts

type VarMap = M.Map (Int, Category) Boolean

buildVarMap :: [Category] -> VarMap
buildVarMap cs =
  let n = length cs - 1
      com = [(i, c) | i <- [0 .. n], c <- cs]
   in M.fromList $ zipWith (\c i -> (c, Var i)) com [0 ..]

equating :: Eq a => (b -> a) -> (b -> b -> Bool)
equating f =
  let f' x y = (f x) == (f y)
   in f'

xorFormula :: VarMap -> [(Int, Category)] -> Boolean
xorFormula vm ks =
  let vars = map (fromJust . ((flip M.lookup) vm)) ks
      atLeastOne = foldr1 (:||:) vars
      varPairs =
        map (map snd) $
          nubBy (equating (map fst)) $
            filter (\l -> length l == 2) $
              map (nubBy (equating fst) . sortBy (comparing fst)) [[v1, v2] | v1 <- zip [0 ..] vars, v2 <- zip [0 ..] vars]
      atMostOne = foldr (:&&:) Yes $ map (\p -> foldr (:||:) No $ map Not p) varPairs
   in atLeastOne :&&: atMostOne

buildPositionConstraints :: VarMap -> (Int, Int) -> Boolean
buildPositionConstraints vm (p, i) =
  let cs = map snd $ filter ((== p) . fst) $ M.keys vm
      vcs = filter (\(c, ui) -> not $ inUnionInterval ui i) cs
   in foldr (:&&:) Yes $ map Not $ map (fromJust . ((flip M.lookup) vm)) $ map (\c -> (p, c)) vcs

buildTicketConstraints :: VarMap -> Ticket -> Boolean
buildTicketConstraints vm t =
  let cs = M.keys vm
      nt = zip [0 ..] t
   in foldr (:&&:) Yes $ map (buildPositionConstraints vm) nt

buildCategoryFormula :: VarMap -> Boolean
buildCategoryFormula vm =
  let ps = nub $ map fst $ M.keys vm
      cs = nub $ map snd $ M.keys vm
      gp = [[(p, c) | c <- cs] | p <- ps]
      gc = [[(p, c) | p <- ps] | c <- cs]
   in foldr (:&&:) Yes $ map (xorFormula vm) (gp ++ gc)

solve2 :: [Category] -> Ticket -> [Ticket] -> IO Int
solve2 cs t ts =
  do
    let varMap = buildVarMap cs
    let f1 = buildCategoryFormula varMap
    let uis = map snd cs
    let vts = filter (not . (invalid uis)) ts
    s1 <- assertTrue f1 newSatSolver :: IO SatSolver
    let f2 = foldr (:&&:) Yes $ map (buildTicketConstraints varMap) (t : vts)
    s2 <- assertTrue f2 s1 :: IO SatSolver
    sf <- solve s2
    let trueVars =
          map show $
            concatMap
              ( \i -> case (lookupVar i sf) of
                  Nothing -> []
                  (Just False) -> []
                  (Just True) -> [Var i]
              )
              [0 .. M.size varMap -1]
    let m = M.keys $ M.filter (\e -> (show e) `elem` trueVars) varMap
    let dp = filter (\(n, p) -> "departure" `isPrefixOf` n) $ map (\(p, (n, _)) -> (n, t !! p)) m

    return $ product $ map snd dp

main = do
  input <- getContents
  let (categories, ticket, nearby) = fromJust $ parseMaybe parseInput input
  --print $ solve1 categories nearby
  i <- solve2 categories ticket nearby
  print $ i

--let nss = newSatSolver
--print nss
--asdf <- assertTrue ((Var 0) :&&: (Var 0)) nss :: IO SatSolver
--print asdf
--asdf' <- solve asdf :: IO SatSolver
--print asdf'

parseInterval :: Parser Interval
parseInterval = do
  l <- L.decimal
  _ <- char '-'
  u <- L.decimal
  return (l, u)

parseUnionInterval :: Parser UnionInterval
parseUnionInterval = do
  a <- parseInterval
  _ <- string " or "
  b <- parseInterval
  return (a, b)

parseCategory :: Parser Category
parseCategory = do
  name <- some (alphaNumChar <|> char ' ')
  _ <- string ": "
  ui <- parseUnionInterval
  return (name, ui)

parseTicket :: Parser Ticket
parseTicket = sepBy L.decimal (char ',')

parseInput :: Parser ([Category], Ticket, [Ticket])
parseInput = do
  categories <- endBy parseCategory eol
  _ <- eol
  _ <- string "your ticket:"
  _ <- eol
  ticket <- parseTicket
  _ <- eol
  _ <- eol
  _ <- string "nearby tickets:"
  _ <- eol
  nearbyTickets <- endBy parseTicket eol
  return (categories, ticket, nearbyTickets)
