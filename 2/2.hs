import Control.Monad
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data PasswordData = P Int Int Char String deriving (Show)

valid1 :: PasswordData -> Bool
valid1 (P a b c s) =
  let count = length $ filter (== c) s
   in count >= a && count <= b

valid2 :: PasswordData -> Bool
valid2 (P a b c s) =
  let b1 = s !! (a - 1) == c
      b2 = s !! (b - 1) == c
   in b1 /= b2

main = do
  input <- getContents
  let (Just x) = parseMaybe (many parsePasswordData) input
  print $ length $ filter valid1 x
  print $ length $ filter valid2 x

lexeme :: Parser a -> Parser a
lexeme = L.lexeme (void (many (char ' ')))

integer :: Parser Int
integer = lexeme L.decimal

parsePasswordData :: Parser PasswordData
parsePasswordData = do
  a <- integer
  _ <- char '-'
  b <- integer
  --  _ <- char ' '
  c <- lowerChar
  _ <- string ": "
  s <- many lowerChar
  _ <- newline
  return (P a b c s)
