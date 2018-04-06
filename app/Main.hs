module Main (main) where

import System.Environment (getArgs)
import System.IO (IOMode (ReadMode), hClose, hGetContents, openFile)

import Lib


main :: IO ()
main = do
  args <- getArgs
  handle <- openFile (head args) ReadMode
  code <- hGetContents handle
  end <- interpret code
  hClose handle

  putStr "END STATE: "
  printStateLn end
