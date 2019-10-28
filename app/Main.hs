module Main where

import           Interpreter (runI)
import           Language (Statement)
import           System.Environment (getArgs)
import           Control.Exception (SomeException, catch)

main :: IO ()
main = do
  [filepath] <- getArgs'
  f <- getFile filepath
  let s = read f :: Statement
  runI s

getFile :: FilePath -> IO String
getFile fp = readFile fp `catch` (\e -> print (e :: SomeException) >> putStrLn loading >> readFile defaultFile) 

getArgs' :: IO [String]
getArgs' = getArgs >>= parse 
  where 
    parse [] = putStrLn loading >> return [defaultFile] 
    parse _  = getArgs

defaultFile :: String
defaultFile = "./test/TestWhile.test"

loading :: String
loading = "Loading Default File"
