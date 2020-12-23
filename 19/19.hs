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
ruleToParser m (Const c) = string [c]
ruleToParser m (Ref i) = ruleToParser m (fromJust $ M.lookup i m)
ruleToParser m (Seq rs) = liftM concat $ mapM (ruleToParser m) rs
ruleToParser m (Alt r1 r2) = try (ruleToParser m r1) <|> try (ruleToParser m r2)

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
        b <- count' 1 (length a - 1) parser31
        return (a, b)
  print $ length $ filter isJust $ map (parseMaybe parser) words
  print $ length $ mapMaybe (parseMaybe parserf) words

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
