module Main where

import System.Environment 
import Language
import Interpreter

main :: IO ()
main = do
  [filepath] <- getArgs
  f <- readFile filepath
  let s = read f :: Statement
  runI s
