module Main where

import Data.Char (chr, ord)
import Data.Int (Int8)
import Data.List (intersperse)
import System.Environment (getArgs)
import qualified Data.Vector.Primitive.Mutable as PM
import System.IO (IOMode (ReadMode), hGetContents, openFile)


-- +-------+
-- + Stack |
-- +-------+

type Stack = PM.IOVector Int8

printStack :: Stack -> IO ()
printStack stack = showStack stack >>= putStrLn

showStack :: Stack -> IO String
showStack stack = cat <$> traverse get [0..end]
  where get :: Int -> IO String
        get index = PM.read stack index >>= return . show
        end = PM.length stack - 1
        cat :: [String] -> String
        cat xs = "[" ++ (concat $ intersperse ", " xs) ++ "]"


-- +-------------+
-- + State & ops |
-- +-------------+

data State = State Stack Int

op :: (Int8 -> IO Int8) -> State -> IO State
op f state@(State stack index) =
  PM.read stack index
  >>= f
  >>= PM.write stack index
  >>  return state

coerce :: (Num c, Integral a) => a -> c
coerce = fromInteger . toInteger

one :: Int8
one = 1

printAsChar :: Int8 -> IO Int8
printAsChar x = (putChar . chr . coerce) x >> return x

readAsInt8 :: Int8 -> IO Int8
readAsInt8 _ = getChar >>= return . coerce . ord

test :: State -> IO Bool
test (State stack index) = (0 /=) <$> PM.read stack index


-- +--------------------------+
-- + Parsing & interpretation |
-- +--------------------------+

extractLoop :: String -> (String, String)
extractLoop = f "" 0
  where f :: String -> Int -> String -> String -> (String, String)
        f loop state ('[':xs) = f (state + 1) xs
        f loop 1     (']':xs) = (loop, xs)
        f loop state (']':xs) = f (state - 1) xs
        f loop state (_:xs)   = f state xs
        f loop _     ""       = (loop, "")

tokenize :: String -> String
tokenize = filter (/= ' ')

interpret' :: String -> State -> IO ()
interpret' "" _ = return ()
interpret'   ('<':xs) (State st i) = interpret' xs (State st $ i - 1)
interpret'   ('>':xs) (State st i) = interpret' xs (State st $ i + 1)
interpret'   ('+':xs) state        = op (return . (one +)) state >>= interpret' xs
interpret'   ('-':xs) state        = op (return . (one -)) state >>= interpret' xs
interpret'   ('.':xs) state        = op printAsChar state >>= interpret' xs
interpret'   (',':xs) state        = op readAsInt8  state >>= interpret' xs
interpret' s@('[':xs) state        = do
  res <- test state
  if res then interpret' xs state
    
interpret'   (_:xs)   state        = interpret' xs state

interpret :: String -> IO ()
interpret code = do
  stack <- PM.replicate 1000 0
  interpret' code (State stack 0)

main :: IO ()
main = do
  args <- getArgs
  handle <- openFile (head args) ReadMode
  code <- hGetContents handle
  interpret code
-- main = getContents >>= interpret
