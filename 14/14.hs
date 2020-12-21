import Control.Monad.State
import Data.Bits
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.String.Utils
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Command = Mask (Int, Int) | Write Int Int deriving (Eq, Show)

type Program = [Command]

type Parser = Parsec Void String

type MState = ((Int, Int), M.Map Int Int)

compose :: [a -> a] -> a -> a
compose = foldl (flip (.)) id

main = do
  input <- getContents
  let (Just program) = parseMaybe parseProgram input
  print $ sum $ map snd $ M.toList $ snd $ evalState (runProgram program) ((0, 0), M.empty)
  print $ sum $ map snd $ M.toList $ snd $ evalState (runProgram' program) ((0, 0), M.empty)

runProgram :: Program -> State MState MState
runProgram [] = get
runProgram (Write a v : ws) = do
        (mask, mem) <- get
        put (mask, M.insert a (applyMask mask v) mem)
        runProgram ws
runProgram (Mask mask' : ws) = do
  (mask, mem) <- get
  put (mask', mem)
  runProgram ws

combinations :: [a] -> Int -> [[a]]
combinations x n = mapM (const x) [1 .. n]

runProgram' :: Program -> State MState MState
runProgram' [] = get
runProgram' (Write a v : ws) =
  do
    (mask@(m0, m1), mem) <- get
    let floating = filter (testBit (complement m1 .&. m0)) [0 .. 35]
    let floatingCount = length floating
    let possibleModifications = combinations [flip clearBit, flip setBit] floatingCount
    let addresses = map ((\f -> f (a .|. m1)) . compose . (\fl -> zipWith (\f i -> f i) fl floating)) possibleModifications
    let writes = zip addresses (repeat v)
    let mem' = foldl (\m (k, v) -> M.insert k v m) mem writes
    put (mask, mem')
    runProgram' ws
runProgram' (Mask mask' : ws) = do
  (mask, mem) <- get
  put (mask', mem)
  runProgram' ws

applyMask :: (Int, Int) -> Int -> Int
applyMask (zeromask, onemask) x = (x .&. zeromask) .|. onemask

integer :: Parser Int
integer = L.decimal

parseMask :: Parser Command
parseMask = do
  _ <- string "mask = "
  s <- many (char 'X' <|> char '0' <|> char '1')
  let ones = map snd $ filter ((== '1') . fst) $ zip (reverse s) [0 ..]
  let zeros = map snd $ filter ((== '0') . fst) $ zip (reverse s) [0 ..]
  let onemask = foldr (flip setBit) 0 ones
  let zeromask = complement $ foldr (flip setBit) 0 zeros
  return $ Mask (zeromask, onemask)

parseWrite :: Parser Command
parseWrite = do
  _ <- string "mem["
  a <- integer
  _ <- string "] = "
  v <- integer
  return $ Write a v

parseProgram :: Parser Program
parseProgram = endBy (parseWrite <|> parseMask) eol
