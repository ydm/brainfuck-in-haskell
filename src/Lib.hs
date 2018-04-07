module Lib where

import Control.Exception (catch)
import Data.Char (chr, ord, isPrint)
import Data.Int (Int8)
import Data.List (intersperse)

import qualified Data.Vector.Primitive.Mutable as PM


-- +-------+
-- + Stack |
-- +-------+

type Stack = PM.IOVector Int8

printStack :: Stack -> IO ()
printStack stack = showStack stack >>= putStr

printStackLn :: Stack -> IO ()
printStackLn stack = printStack stack >> putStrLn ""

showStack :: Stack -> IO String
showStack stack = cat <$> traverse get [0..end]
  where get :: Int -> IO String
        get index = PM.read stack index >>= return . show
        end :: Int
        end = PM.length stack - 1
        cat :: [String] -> String
        cat xs = "[" ++ (concat $ intersperse ", " xs) ++ "]"


-- +-------------+
-- + State & ops |
-- +-------------+

data State = State Stack Int

printStateLn :: State -> IO ()
printStateLn (State stack index) = printStack stack >> putStr ", " >> print index

op :: (Int8 -> IO Int8) -> State -> IO State
op f state@(State stack index) =
  PM.read stack index
  >>= f
  >>= PM.write stack index
  >>  return state

plus :: Int8 -> IO Int8
plus x = return $ x + (1 :: Int8)

minus :: Int8 -> IO Int8
minus x = return $ x - (1 :: Int8)

coerce :: (Num c, Integral a) => a -> c
coerce = fromInteger . toInteger

printAsChar :: Int8 -> IO Int8
printAsChar x
  | 32 <= x && x <= 126 = (putChar . chr . coerce) x >> return x
  | otherwise         = putStr "\\" >> putStr (show x) >> return x

readAsInt8 :: Int8 -> IO Int8
readAsInt8 _ = catch (getChar >>= return . coerce . ord) f
  where f :: (IOError -> IO Int8)
        f _ = return 0

testCondition :: State -> IO Bool
testCondition (State stack index) = (0 /=) <$> PM.read stack index


-- +--------------------------+
-- + Parsing & interpretation |
-- +--------------------------+

-- | Split the given code string to loop body and what's after that.
extractLoop :: String -> (String, String)
extractLoop = rev <$> f "" 0
  where f :: String -> Int -> String -> (String, String)
        f  "" n ('[':xs) = f       ""  (n + 1) xs
        f acc n ('[':xs) = f ('[':acc) (n + 1) xs
        f acc 1 (']':xs) =  ((']':acc), xs)
        f acc n (']':xs) = f (']':acc) (n - 1) xs
        f acc n (x:xs)   = f (x:acc) n xs
        f acc _ ""       = (acc, "")
        rev (a, b) = (reverse a, b)

interpret' :: String -> State -> IO State
interpret'      ""   state       = return state
interpret'   ('<':xs) (State st i) = interpret' xs $ State st $ i - 1
interpret'   ('>':xs) (State st i) = interpret' xs $ State st $ i + 1
interpret'   ('+':xs)  state       = op plus        state >>= interpret' xs
interpret'   ('-':xs)  state       = op minus       state >>= interpret' xs
interpret'   ('.':xs)  state       = op printAsChar state >>= interpret' xs
interpret'   (',':xs)  state       = op readAsInt8  state >>= interpret' xs
interpret' s@('[':_ )  state       = interpretLoop (extractLoop s) state
interpret'   ('!':xs)  state       = printStateLn state >> interpret' xs state
interpret'   (  _:xs)  state       = interpret' xs state

interpretLoop :: (String, String) -> State -> IO State
interpretLoop tup@(body, rest) state = do
  res <- testCondition state
  if res
    then interpret' body state >>= interpretLoop tup
    else interpret' rest state

interpret :: String -> IO State
interpret code = do
  stack <- PM.replicate 10 0
  interpret' code (State stack 0)
