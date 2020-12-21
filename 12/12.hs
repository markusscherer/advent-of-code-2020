import Control.Monad.State
import Data.List
import Data.Maybe
import Data.String.Utils
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data Direction = North | West | South | East deriving (Show, Eq)

data TurnDirection = L | R deriving (Show, Eq)

data Command = Absolut Direction Int | Forward Int | Rotate TurnDirection Int deriving (Show)

type BoatState = (Direction, Int, Int)

type BoatState' = ((Int, Int), (Int, Int))

rotate R d i =
  let l = [North, East, South, West]
      (Just dd) = elemIndex d l
      ddd = (i + dd + 4) `mod` 4
   in l !! ddd
rotate L d i =
  let l = reverse [North, East, South, West]
      (Just dd) = elemIndex d l
      ddd = (i + dd + 4) `mod` 4
   in l !! ddd

rotate' (x, y) R 1 = (y, - x)
rotate' (x, y) L 1 = (- y, x)
rotate' p d c = rotate' (rotate' p d (c -1)) d 1

applyCommand :: Command -> BoatState -> BoatState
applyCommand (Absolut North i) (d, x, y) = (d, x, y + i)
applyCommand (Absolut West i) (d, x, y) = (d, x - i, y)
applyCommand (Absolut South i) (d, x, y) = (d, x, y - i)
applyCommand (Absolut East i) (d, x, y) = (d, x + i, y)
applyCommand (Forward i) bs@(d, x, y) = applyCommand (Absolut d i) bs
applyCommand (Rotate t i) (d, x, y) = (rotate t d (i `div` 90), x, y)

runCommands :: [Command] -> State BoatState BoatState
runCommands [] = get
runCommands (c : cs) = do
  bs <- get
  put $ applyCommand c bs
  runCommands cs

applyCommand' :: Command -> BoatState' -> BoatState'
applyCommand' (Absolut North i) ((wx, wy), (x, y)) = ((wx, wy + i), (x, y))
applyCommand' (Absolut West i) ((wx, wy), (x, y)) = ((wx - i, wy), (x, y))
applyCommand' (Absolut South i) ((wx, wy), (x, y)) = ((wx, wy - i), (x, y))
applyCommand' (Absolut East i) ((wx, wy), (x, y)) = ((wx + i, wy), (x, y))
applyCommand' (Forward i) ((wx, wy), (x, y)) = ((wx, wy), (x + i * wx, y + i * wy))
applyCommand' (Rotate t i) (wp, (x, y)) = (rotate' wp t (i `div` 90), (x, y))

runCommands' :: [Command] -> State BoatState' BoatState'
runCommands' [] = get
runCommands' (c : cs) = do
  bs <- get
  put $ applyCommand' c bs
  runCommands' cs

main = do
  input <- getContents
  --parseTest (endBy parseCommand eol) input
  let (Just cmds) = parseMaybe (endBy parseCommand eol) input
  -- print $ evalState (runCommands cmds) (East, 0, 0)
  print $ evalState (runCommands' cmds) ((10, 1), (0, 0))

parseCommand :: Parser Command
parseCommand = parseAbsolut <|> parseForward <|> parseRotate

integer :: Parser Int
integer = L.decimal

parseDirection :: Parser Direction
parseDirection = do
  n <- choice $ map char "NWSE"
  return $ case n of
    'N' -> North
    'W' -> West
    'E' -> East
    'S' -> South

parseAbsolut :: Parser Command
parseAbsolut = do
  d <- parseDirection
  i <- integer
  return $ Absolut d i

parseTurnDirection :: Parser TurnDirection
parseTurnDirection = do
  n <- choice $ map char "LR"
  return $ case n of
    'L' -> L
    'R' -> R

parseForward :: Parser Command
parseForward = do
  _ <- char 'F'
  i <- integer
  return $ Forward i

parseRotate :: Parser Command
parseRotate = do
  t <- parseTurnDirection
  i <- integer
  return $ Rotate t i
