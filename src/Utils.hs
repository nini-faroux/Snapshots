{-# LANGUAGE FlexibleContexts #-}

module Utils where 

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Except   (MonadError, throwError)
import qualified Data.Map     as Map
import Language               (Statement, Name, Val, Env)

-- Interpreter Error 

newtype IError = IError String deriving Show

evalError :: (MonadError IError m, MonadIO m) => String -> m a 
evalError errMsg = do
    printI errMsg
    throwError (IError errMsg)

-- Interpreter Print Helpers 

printStack :: (MonadIO m, Show p, Show s) => [(p, s)] -> m () 
printStack [] = printIS lines' >> printIS "Empty Stack" >> printIS lines'
printStack xs = printIS lines' >> foldr (\(pos, stmt) -> (>>) (liftIO (printIS (nameVar pos stmt) >> newLine))) (return ()) xs >> printIS lines' 
    where 
      nameVar position statment = "<" ++ show position ++ "> :" ++ show statment 

printMap :: (MonadIO m, Show k, Show v) => [(k, v)] -> m () 
printMap []  = printIS lines' >> printIS "Empty Environment" >> printIS lines'
printMap env = printIS lines' >> foldr (\(name, var) -> (>>) (liftIO (printIS (nameEqualVar name var) >> newLine))) (return ()) env >> printIS lines'
    where 
      nameEqualVar name var = show name ++ " = " ++ show var 

printVHist :: (MonadIO m, Show k, Show k', Show v) => [(k, Map.Map k' v)] -> m () 
printVHist []  = printIS lines' >> printIS "No Variable History" >> printIS lines'
printVHist env = printIS lines' >> foldr (\(name, var) -> (>>) (liftIO (printIS (nameEqualVar name var) >> newLine))) (return ()) env >> printIS lines'
    where 
      nameEqualVar name var = "<" ++ show name ++ "> : " ++ showTuples (Map.toList var) 

showTuples :: (Show k, Show v) => [(k, v)] -> String
showTuples []     = ""
showTuples (x:xs) = show (fst x) ++ " = " ++ show (snd x) ++ "; " ++ showTuples xs

printI :: (MonadIO m, Show a) => a -> m ()
printI xs = liftIO $ newLine >> print xs >> newLine

printIS :: MonadIO m => String -> m () 
printIS xs = liftIO $ newLine >> putStrLn xs >> newLine

liftPutStrLn :: MonadIO m => String -> m () 
liftPutStrLn xs = liftIO $ putStrLn xs 

-- Interpreter Display 

mainDisplay :: MonadIO m => m () 
mainDisplay = do 
  printIS stars' 
  liftPutStrLn   "<n> : Execute Next Instruction                          *"
  printDisplay   "<b> : Step Back in Program's Exeuction                  *" 
  printDisplay   "<i> : Inspect State of Current Variables                *" 
  printDisplay   "<s> : View Previously Executed Statments                *"
  printDisplay   "<v> : View State of Variables at each Instruction       *" 
  printDisplay   "<q> : Quit Program                                      *"
  printIS stars' 

printWaitLoop :: MonadIO m => m () 
printWaitLoop = printIS "<e> : Return to Options"

printDisplay :: MonadIO m => String -> m () 
printDisplay xs = printStarEnd >> liftPutStrLn xs

printEnv :: MonadIO m => Env -> m () 
printEnv env 
  | Map.null env = printIS lines' >> printIS "No Variables Assigned" >> printIS lines'
  | otherwise    = printIS lines' >> (printIS . showTuples $ Map.toList env) >> printIS lines'

displayVar :: MonadIO m => Name -> Val -> m ()
displayVar name val = printI $ "- " ++ "Value " ++ show val ++ " assigned to variable " ++ show name

printAtInstruction :: MonadIO m => Statement -> m () 
printAtInstruction s = printIS ("<Next Instruction> : " ++ show s) 

printExecuting :: MonadIO m => Statement -> m () 
printExecuting s = printIS lines' >> printIS ("<Executing instruction> : " ++ show s) >> printIS lines'

printVariable :: MonadIO m => Name -> Val -> m () 
printVariable name val = printI $ "Variable " ++ name ++ " = " ++ show val

printNotFound :: MonadIO m => m () 
printNotFound = printIS "Variable not found"

printAtStart :: MonadIO m => m () 
printAtStart = printIS "Can't go back"

printInvalid :: MonadIO m => m () 
printInvalid = printIS "Error: Invalid Command"

quitting :: MonadIO m => m () 
quitting = printIS "Quitting..."

printStarEnd :: MonadIO m => m () 
printStarEnd = printIS starEnd

-- File loading helpers 

getCommand :: MonadIO m => m String
getCommand = liftIO getLine

newLine :: IO () 
newLine = putStrLn ""

defaultFile :: String
defaultFile = "./test/TestAll.test"

loadingFile :: IO ()
loadingFile = putStrLn "Loading Default file   *"

printStars :: IO () 
printStars = putStrLn stars 

starEnd :: String 
starEnd = "                                                        *"

stars' :: String 
stars' = "*********************************************************"

stars :: String
stars = "************************"

lines' :: String 
lines' = "=============================================================================================="
