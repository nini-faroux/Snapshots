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
getFile fp = readFile fp `catch` (\e -> print (e :: SomeException) >> loading >> readFile defaultFile) 

getArgs' :: IO [String]
getArgs' = getArgs >>= parse 
  where 
    parse [] = loading >> return [defaultFile] 
    parse _  = getArgs

loading :: IO () 
loading = newLine >> stars >> loadF >> stars

loadF :: IO ()
loadF = putStrLn "Loading Default file"

stars :: IO ()
stars = putStrLn "********************"

newLine :: IO () 
newLine = putStrLn ""

defaultFile :: String
defaultFile = "./test/TestWhile.test"
