{-# LANGUAGE FlexibleContexts #-}

module Utils where

import           Control.Monad.Except   (MonadError, throwError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map               as Map
import           Language               (Env, Name, Statement, Val)
import           System.Console.ANSI    (clearScreen)

-- | Interpreter Error
newtype IError = IError String deriving Show

evalError :: (MonadError IError m, MonadIO m) => String -> m a
evalError errMsg = do
    printI errMsg
    throwError (IError errMsg)

-- | Lift clearScreen from Console library
liftClearScreen :: MonadIO m => m ()
liftClearScreen = liftIO clearScreen

-- | Interpreter Print Helpers
printDS :: (MonadIO m, Show a, Show b) => [(a, b)] -> String -> (a -> b -> String) -> m ()
printDS [] s _           = linesMessageLines s
printDS xs _ helperPrint = printLines >> foldr (\(a, b) -> (>>) (liftIO (printIS (helperPrint a b) >> newLine))) (return ()) xs >> printLines

printStack :: (MonadIO m, Show p, Show s) => [(p, s)] -> m ()
printStack xs = printDS xs "Empty Stack" posStmt
  where
    posStmt p s = "<" ++ show p ++ "> :" ++ show s

printVHist :: (MonadIO m, Show k, Show k', Show v) => [(k, Map.Map k' v)] -> m ()
printVHist xs = printDS xs "No Variable History" nameVar
  where
    nameVar name var = "<" ++ show name ++ "> : " ++ showTuples (Map.toList var)

showTuples :: (Show k, Show v) => [(k, v)] -> String
showTuples []     = ""
showTuples (x:xs) = show (fst x) ++ " = " ++ show (snd x) ++ "; " ++ showTuples xs

printI :: (MonadIO m, Show a) => a -> m ()
printI xs = liftIO $ newLine >> print xs >> newLine

printIS :: MonadIO m => String -> m ()
printIS xs = liftIO $ newLine >> putStrLn xs >> newLine

linesMessageLines :: MonadIO m => String -> m ()
linesMessageLines s = printLines >> printIS s >> printLines

liftPutStrLn :: MonadIO m => String -> m ()
liftPutStrLn xs = liftIO $ putStrLn xs

-- | Interpreter Display
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
  | Map.null env = linesMessageLines "No Variables Assigned"
  | otherwise    = printLines >> (printIS . showTuples $ Map.toList env) >> printLines

displayVar :: MonadIO m => Name -> Val -> m ()
displayVar name val = printI $ "- " ++ "Value " ++ show val ++ " assigned to variable " ++ show name

printBackSuccess :: MonadIO m => Statement -> m ()
printBackSuccess stmt = linesMessageLines ("<Back To> : " ++ show stmt)

printAtInstruction :: MonadIO m => Statement -> m ()
printAtInstruction s = printIS ("<Next Instruction> : " ++ show s)

printExecuting :: MonadIO m => Statement -> m ()
printExecuting s = linesMessageLines ("<Executing instruction> : " ++ show s)

printVariable :: MonadIO m => Name -> Val -> m ()
printVariable name val = printI $ "Variable " ++ name ++ " = " ++ show val

printNotFound :: MonadIO m => m ()
printNotFound = printIS "Variable not found"

printAtStart :: MonadIO m => m ()
printAtStart = linesMessageLines "Already at Start - Can't go Back"

printInvalid :: MonadIO m => m ()
printInvalid = printIS "Error: Invalid Command"

quitting :: MonadIO m => m ()
quitting = printIS "Quitting..."

printStarEnd :: MonadIO m => m ()
printStarEnd = printIS starEnd

printLines :: MonadIO m => m ()
printLines = printIS lines'

-- | File loading helpers
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
