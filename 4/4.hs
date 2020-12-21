import Data.List
import Data.Maybe
import Data.String.Utils
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Field = (String, String)

type PassportData = [Field]

type Parser = Parsec Void String

keys = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid"]

keysWoCid = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

valid :: PassportData -> Bool
valid pd = all (\k -> any (\(kk, vv) -> kk == k) pd) keysWoCid

isBetween :: Int -> Int -> Int -> Bool
isBetween lower upper x = lower <= x && x <= upper

ensure :: (a -> Bool) -> a -> Maybe a
ensure predicate x = if predicate x then Just x else Nothing

valid2 :: Field -> Bool
valid2 ("byr", s) = isJust $ maybeRead s >>= ensure (isBetween 1920 2002)
valid2 ("iyr", s) = isJust $ maybeRead s >>= ensure (isBetween 2010 2020)
valid2 ("eyr", s) = isJust $ maybeRead s >>= ensure (isBetween 2020 2030)
valid2 ("hgt", s)
  | endswith "cm" s = isJust $ maybeRead (init (init s)) >>= ensure (isBetween 150 193)
  | endswith "in" s = isJust $ maybeRead (init (init s)) >>= ensure (isBetween 59 76)
  | otherwise = False
valid2 ("hcl", s) = length s == 7 && head s == '#' && all (`elem` "0123456789abcdef") (tail s)
valid2 ("ecl", s) = s `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
valid2 ("pid", s) = length s == 9 && all (`elem` "0123456789") s
valid2 ("cid", s) = True
valid2 (k, s) = error $ "(" ++ k ++ ", " ++ s ++ ")"

main = do
  input <- getContents
  --  parseTest (sepBy parsePasswordData eol) input
  let (Just x) = parseMaybe (sepBy parsePasswordData eol) input
  --  putStr $ intercalate "\n" $ map show $ filter valid x
  print $ length $ filter (all valid2) $ filter valid x

parseField :: Parser Field
parseField = do
  key <- choice $ map string keys
  _ <- char ':'
  value <- many (alphaNumChar <|> char '#')
  _ <- spaceChar
  return (key, value)

parsePasswordData :: Parser PassportData
parsePasswordData = many parseField
