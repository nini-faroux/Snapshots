module Main (main) where

import           Control.Exception  (SomeException, catch)
import           Control.Concurrent (threadDelay)
import           Interpreter        (runI)
import           Language           (Statement)
import           System.Environment (getArgs)
import           Display

-- | Program entry point
main :: IO ()
main = do
  setTerminal
  [filepath] <- getArgs'
  f <- getFile filepath
  let s = read f :: Statement
  runI s

-- | Loads file given by value of FilePath
-- If invalid path given catches exception
-- and loads default file
getFile :: FilePath -> IO String
getFile fp = readFile fp `catch` (\e -> print (e :: SomeException) >> loading >> readFile defaultFile)

-- | Reads in user input
-- Returns default file if no user input given
getArgs' :: IO [String]
getArgs' = getArgs >>= parse
  where
    parse [] = loading >> return [defaultFile]
    parse _  = getArgs

loading :: IO ()
loading = newLine >> printHashes >> loadingFile >> printHashes >> threadDelay 990000
