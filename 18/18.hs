import Control.Monad.Combinators.Expr
import Data.Maybe
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Expr = Const Int | Plus Expr Expr | Mul Expr Expr deriving (Show, Eq)

eval :: Expr -> Int
eval (Const i) = i
eval (Plus a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

type Parser = Parsec Void String

main = do
  input <- getContents
  let exprs = fromJust $ parseMaybe (endBy parseExpr eol) input
  print $ sum $ map eval exprs
  let exprs' = fromJust $ parseMaybe (endBy parseExpr' eol) input
  print $ sum $ map eval exprs'

parseConst :: Parser Expr
parseConst = do
  i <- L.decimal
  _ <- many (char ' ')
  return $ Const i

symbol = L.symbol (many (char ' ') >> return ())

parseParen :: Parser Expr
parseParen = between (symbol "(") (symbol ")") parseExpr

parseTerm :: Parser Expr
parseTerm = parseConst <|> parseParen

parseExpr :: Parser Expr
parseExpr = makeExprParser parseTerm [[InfixL (Plus <$ symbol "+"), InfixL (Mul <$ symbol "*")]]

parseParen' :: Parser Expr
parseParen' = between (symbol "(") (symbol ")") parseExpr'

parseTerm' :: Parser Expr
parseTerm' = parseConst <|> parseParen'

parseExpr' :: Parser Expr
parseExpr' = makeExprParser parseTerm' [[InfixL (Plus <$ symbol "+")], [InfixL (Mul <$ symbol "*")]]
