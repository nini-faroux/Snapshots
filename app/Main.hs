module Main where

import           Interpreter (runI)
import           Language (Statement)
import           System.Environment (getArgs)

main :: IO ()
main = do
  [filepath] <- getArgs
  f <- readFile filepath
  let s = read f :: Statement
  runI s
