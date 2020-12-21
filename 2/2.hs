import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

--import Text.Megaparsec.String
type Parser = Parsec Void String

data PasswordData = P Int Int Char String deriving (Show)

sc :: Parser ()
sc =
  L.space
    space1 -- (2)
    (L.skipLineComment "//") -- (3)
    (L.skipBlockComment "/*" "*/") -- (4)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

valid :: PasswordData -> Bool
valid (P a b c s) =
  let count = length $ filter (== c) s
   in count >= a && count <= b

valid2 :: PasswordData -> Bool
valid2 (P a b c s) =
  let b1 = s !! (a - 1) == c
   in let b2 = s !! (b - 1) == c
       in b1 /= b2

main = do
  input <- getContents
  let (Just x) = parseMaybe (many parsePasswordData) input
  putStr $ show $ length $ filter valid2 x

integer :: Parser Int
integer = lexeme L.decimal

lower = choice $ map char "abcdefghijklmnopqrstuvwxyz"

parsePasswordData :: Parser PasswordData
parsePasswordData = do
  a <- integer
  _ <- char '-'
  b <- integer
  --  _ <- char ' '
  c <- lower
  _ <- string ": "
  s <- many lower
  _ <- newline
  return (P a b c s)
