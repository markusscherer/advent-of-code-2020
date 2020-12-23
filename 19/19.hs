import Control.Monad
import Control.Monad.Combinators.Expr
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug

data Rule = Const Char | Ref Int | Seq [Rule] | Alt Rule Rule deriving (Show, Eq)

type Parser = Parsec Void String

ruleToParser :: M.Map Int Rule -> Rule -> Parser String
ruleToParser m (Const c) = do
  c' <- char c
  return [c]
ruleToParser m (Ref i) = do
  let r = fromJust $ M.lookup i m
  ruleToParser m r
ruleToParser m (Seq rs) = do
  r <- mapM (ruleToParser m) rs
  return $ concat r
ruleToParser m (Alt r1 r2) = try (ruleToParser m r1) <|> try (ruleToParser m r2)

showRule (Const c) = '"' : c : "\""
showRule (Ref i) = "ref " ++ show i
showRule (Alt r1 r2) = "(" ++ showRule r1 ++ " | " ++ showRule r2 ++ ")"
showRule (Seq rs) = unwords $ map showRule rs

generateStrings :: Rule -> [String]
generateStrings (Const c) = [[c]]
generateStrings (Ref i) = ["(" ++ show i ++ ")"]
generateStrings (Alt r1 r2) = nub $ do
  s1 <- generateStrings r1
  s2 <- generateStrings r2
  [s1, s2]
generateStrings (Seq rs) = nub $ map concat $ mapM generateStrings rs

inlineRefs :: M.Map Int Rule -> [Int] -> Rule -> Rule
inlineRefs m ex r@(Ref i) = if i `elem` ex then r else inlineRefs m ex (fromJust $ M.lookup i m)
inlineRefs m ex (Seq rs) = Seq $ map (inlineRefs m ex) rs
inlineRefs m ex (Alt r1 r2) = Alt (inlineRefs m ex r1) (inlineRefs m ex r2)
inlineRefs m ex (Const c) = Const c

lexeme :: Parser a -> Parser a
lexeme = L.lexeme (void (many (char ' ')))

main = do
  input <- getContents
  let (ruleLines, words) = fromJust $ parseMaybe parseInput input
  let ruleMap = M.fromList ruleLines
  let parser = ruleToParser ruleMap $ fromJust $ M.lookup 0 ruleMap
  -- the following three parsers were generated after manual inspection
  let parser42 = ruleToParser ruleMap $ fromJust $ M.lookup 42 ruleMap
  let parser31 = ruleToParser ruleMap $ fromJust $ M.lookup 31 ruleMap
  let parserf = do
        a <- count' 2 10000 parser42
        b <- count' 1 10000 parser31
        return (a, b)
  print $ length $ filter isJust $ map (parseMaybe parser) words
  print $ length $ filter (\(a, b) -> length a > length b) $ mapMaybe (parseMaybe parserf) words

parseConst :: Parser Rule
parseConst = lexeme $ between (char '"') (char '"') ((char 'a' <|> char 'b') >>= (return . Const))

parseRef :: Parser Rule
parseRef = lexeme $ L.decimal >>= (return . Ref)

parseSeq :: Parser Rule
parseSeq = lexeme $ count' 2 10000 parseRef >>= (return . Seq)

parseSimple :: Parser Rule
parseSimple = parseConst <|> try parseSeq <|> try parseRef

parseAlt :: Parser Rule
parseAlt = lexeme $ do
  r1 <- parseSimple
  _ <- lexeme (char '|')
  Alt r1 <$> parseSimple

parseRule :: Parser Rule
parseRule = try parseAlt <|> parseSimple

parseRuleLine :: Parser (Int, Rule)
parseRuleLine = do
  i <- lexeme L.decimal
  _ <- lexeme (char ':')
  r <- parseRule
  return (i, r)

parseInput :: Parser ([(Int, Rule)], [String])
parseInput = do
  rs <- endBy parseRuleLine eol
  _ <- eol
  ws <- endBy (many (char 'a' <|> char 'b')) eol
  return (rs, ws)
