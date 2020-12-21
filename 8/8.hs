import Control.Monad
import Control.Monad.State
import Data.List
import Data.Maybe
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

type MState = (Int, Int, [Int])

data Opcode = Acc Int | Nop Int | Jmp Int deriving (Show)

nextState :: Opcode -> MState -> MState
nextState (Acc i) (acc, pc, visited) = (acc + i, pc + 1, pc : visited)
nextState (Jmp i) (acc, pc, visited) = (acc, pc + i, pc : visited)
nextState (Nop _) (acc, pc, visited) = (acc, pc + 1, pc : visited)

runCode :: [Opcode] -> State MState Int
runCode code = do
  s@(acc, pc, visited) <- get
  let op = code !! pc
  if pc `elem` visited
    then return acc
    else put (nextState op s) >> runCode code

runCode' :: [Opcode] -> State MState (Maybe Int)
runCode' code = do
  s@(acc, pc, visited) <- get
  let terminated = length code <= pc || pc < 0
  if terminated
    then return (Just acc)
    else
      if pc `elem` visited
        then return Nothing
        else
          let op = code !! pc
           in put (nextState op s) >> runCode' code

opCodeVariationsImpl :: [Opcode] -> [Opcode] -> [[Opcode]]
opCodeVariationsImpl [] rev = []
opCodeVariationsImpl (op@(Jmp i) : ops) rev = (reverse rev ++ Nop i : ops) : opCodeVariationsImpl ops (op : rev)
opCodeVariationsImpl (op@(Nop i) : ops) rev = (reverse rev ++ Jmp i : ops) : opCodeVariationsImpl ops (op : rev)
opCodeVariationsImpl (op@(Acc i) : ops) rev = opCodeVariationsImpl ops (op : rev)

opCodeVariations :: [Opcode] -> [[Opcode]]
opCodeVariations ops = opCodeVariationsImpl ops []

main = do
  input <- getContents
  let (Just ops) = parseMaybe (many parseOpcode) input
  --  print $ evalState (runCode ops) (0, 0, [])
  print $ filter isJust $ map (\ops' -> evalState (runCode' ops') (0, 0, [])) (opCodeVariations ops)
  -- putStrLn $ intercalate "\n" $ map show (opCodeVariations ops)

--  parseTest (many parseOpcode) input

parseOpcode :: Parser Opcode
parseOpcode = parseAcc <|> parseNop <|> parseJmp

parseAcc :: Parser Opcode
parseAcc = do
  _ <- string "acc "
  i <- L.signed (void (string "")) L.decimal
  _ <- eol
  return $ Acc i

parseNop :: Parser Opcode
parseNop = do
  _ <- string "nop "
  i <- L.signed (void (string "")) L.decimal
  _ <- eol
  return $ Nop i

parseJmp :: Parser Opcode
parseJmp = do
  _ <- string "jmp "
  i <- L.signed (void (string "")) L.decimal
  _ <- eol
  return $ Jmp i
