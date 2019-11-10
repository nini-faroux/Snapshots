{-# LANGUAGE FlexibleContexts #-}

module Utils where 

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Except   (MonadError, throwError)
import Language               (Statement, Name, Val)

-- Interpreter Error 

newtype IError = IError String deriving Show

evalError :: (MonadError IError m, MonadIO m) => String -> m a 
evalError errMsg = do
    printI errMsg
    throwError (IError errMsg)

-- Interpreter Print Helpers 

printStack :: (MonadIO m, Show a) => [a] -> m () 
printStack xs = printI stars >> foldr (\x -> (>>) (liftIO (print x >> newLine))) (return ()) xs >> printI stars 

printI :: (MonadIO m, Show a) => a -> m ()
printI xs = liftIO $ newLine >> print xs >> newLine

printI' :: MonadIO m => String -> m () 
printI' xs = liftIO $ newLine >> putStrLn xs >> newLine

-- Interpreter Display 

mainDisplay :: MonadIO m => m () 
mainDisplay = do 
  printI' stars' 
  liftPutStrLn   "<n> : Execute Next Instruction                          *"
  printDisplay   "<b> : Step Back in Program's Exeuction                  *" 
  printDisplay   "<i> : Inspect State of Current Variables                *" 
  printDisplay   "<s> : View Statements Executed                          *"
  printDisplay   "<v> : View State of Variables at each Instruction       *" 
  printI' stars' 

printDisplay :: MonadIO m => String -> m () 
printDisplay xs = printStarEnd >> liftIO (putStrLn xs) 

liftPutStrLn :: MonadIO m => String -> m () 
liftPutStrLn xs = liftIO $ putStrLn xs 

displayVar :: MonadIO m => Name -> Val -> m ()
displayVar name val = printI $ "- " ++ "Value " ++ show val ++ " assigned to variable " ++ show name

printAtInstruction :: MonadIO m => Statement -> m () 
printAtInstruction s = printI' ("Next Instruction: " ++ show s)

printExecuting :: MonadIO m => Statement -> m () 
printExecuting s = printI $ "Executing instruction: " ++ show s

printVariable :: MonadIO m => Name -> Val -> m () 
printVariable name val = printI $ "Variable " ++ name ++ " = " ++ show val

printNotFound :: MonadIO m => m () 
printNotFound = printI' "Variable not found"

printAtStart :: MonadIO m => m () 
printAtStart = printI' "Can't go back"

printInvalid :: MonadIO m => m () 
printInvalid = printI' "Error: Invalid Command"

printStarEnd :: MonadIO m => m () 
printStarEnd = printI' starEnd

-- File loading helpers 

printStars :: IO () 
printStars = putStrLn stars 

newLine :: IO () 
newLine = putStrLn ""

defaultFile :: String
defaultFile = "./test/TestAll.test"

loadingFile :: IO ()
loadingFile = putStrLn "Loading Default file   *"

stars' :: String 
stars' = "*********************************************************"

starEnd :: String 
starEnd = "                                                        *"

stars :: String
stars = "************************"
