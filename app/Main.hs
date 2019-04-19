module Main where

import           Interpreter
import           Language
import           System.Environment

main :: IO ()
main = do
  [filepath] <- getArgs
  f <- readFile filepath
  let s = read f :: Statement
  runI s
