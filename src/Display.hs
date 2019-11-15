{-# LANGUAGE FlexibleContexts #-}

module Display where

import           Control.Monad.Except   (MonadError, throwError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map               as Map
import           Language               (Env, Name, Statement, Val)
import           System.Console.ANSI

-- | Ansi-console functions
liftSetColour :: MonadIO m => ColorIntensity -> Color -> m ()
liftSetColour intensity colour = liftIO $ setSGR [SetColor Foreground intensity colour]

setCyan :: MonadIO m => m ()
setCyan = liftSetColour Vivid Cyan

setDullCyan :: MonadIO m => m ()
setDullCyan = liftSetColour Dull Cyan

setMagenta :: MonadIO m => m ()
setMagenta = liftSetColour Vivid Magenta

setRed :: MonadIO m => m ()
setRed = liftSetColour Vivid Red

liftClearScreen :: MonadIO m => m ()
liftClearScreen = liftIO clearScreen

liftScrollUp :: MonadIO m => Int -> m ()
liftScrollUp n = liftIO $ scrollPageUp n

setTerminal :: IO ()
setTerminal = do
  setSGR [SetColor Foreground Vivid Red]
  setTitle "Snapshots"

-- | Interpreter Print Helpers
printDS :: (MonadIO m, Show a, Show b) => [(a, b)] -> String -> (a -> b -> String) -> m ()
printDS [] s _           = linesMessageLines s
printDS xs _ helperPrint = printLines >> foldr (\(a, b) -> (>>) (liftIO (printIS (helperPrint a b) >> newLine))) (return ()) xs >> printLines

printStack :: (MonadIO m, Show p, Show s) => [(p, s)] -> m ()
printStack xs = liftSetColour Vivid Cyan >> printDS xs "No Instruction History" posStmt
  where
    posStmt p s = "<" ++ show p ++ "> :" ++ show s

printVHist :: (MonadIO m, Show k, Show k', Show v) => [(k, Map.Map k' v)] -> m ()
printVHist xs = setDullCyan >> printDS xs "No Variable History" nameVar
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
mainDisplay :: MonadIO m => Statement -> m ()
mainDisplay stmt = do
  liftPrintStars
  setRed
  liftPutStrLn   "<n>  : Execute Next Instruction                          *"
  printDisplay   "<b>  : Step Back in Program's Exeuction                  *"
  printDisplay   "<vn> : View Next Instruction                             *"
  printDisplay   "<i>  : Inspect State of Current Variables                *"
  printDisplay   "<iv> : Inspect a specific Variable                       *"
  printDisplay   "<s>  : View Previously Executed Statments                *"
  printDisplay   "<v>  : View State of Variables at each Instruction       *"
  printDisplay   "<q>  : Quit Program                                      *"
  liftPrintStars
  liftScrollUp 2

printWaitLoop :: MonadIO m => m ()
printWaitLoop = setCyan >> printIS "<e> : Return to Options"

printDisplay :: MonadIO m => String -> m ()
printDisplay xs = printStarEnd >> liftPutStrLn xs

printEnv :: MonadIO m => Env -> m ()
printEnv env
  | Map.null env = setDullCyan >> linesMessageLines "No Variables Assigned"
  | otherwise    = setDullCyan >> printLines >> (printIS . showTuples $ Map.toList env) >> printLines

printVar :: MonadIO m => Env -> m ()
printVar env = do
  setCyan
  linesMessageLines "Enter the Variable name"
  name <- getCommand
  case Map.lookup name env of
    Nothing    -> printNotFound name
    (Just val) -> printVarFound name val

printVarFound :: MonadIO m => Name -> Val -> m ()
printVarFound name val = do
  liftClearScreen
  printLines
  liftPutStrLn $ "Variable " ++ show name ++ " = " ++ show val
  printLines

printNotFound :: MonadIO m => Name -> m ()
printNotFound name = do
  liftClearScreen
  printLines
  liftPutStrLn $ "Variable " ++ show name ++ " not found"
  printLines

printBackSuccess :: MonadIO m => Statement -> m ()
printBackSuccess stmt = setDullCyan >> linesMessageLines ("<Back To> : " ++ show stmt)

printAtInstruction :: MonadIO m => Statement -> m ()
printAtInstruction s = do
  liftSetColour Vivid Cyan
  linesMessageLines ("<Next Instruction> : " ++ show s)

printExecuting :: MonadIO m => Statement -> m ()
printExecuting s = do
  liftSetColour Dull Cyan
  linesMessageLines ("<Executing instruction> : " ++ show s)

printVariable :: MonadIO m => Name -> Val -> m ()
printVariable name val = printI $ "Variable " ++ name ++ " = " ++ show val

printAtStart :: MonadIO m => m ()
printAtStart = setRed >> linesMessageLines "Already at Start - Can't go Back"

printInvalid :: MonadIO m => m ()
printInvalid = setDullCyan >> printIS "Error: Invalid Command"

liftPrintStars :: MonadIO m => m ()
liftPrintStars = setMagenta >> printIS stars'

quitting :: MonadIO m => m ()
quitting = setRed >> printIS "Quitting..."

printStarEnd :: MonadIO m => m ()
printStarEnd = do
  setMagenta
  printIS starEnd
  setRed

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
loadingFile = putStrLn "Loading Default file   #"

printStars :: IO ()
printStars = putStrLn stars

printHashes :: IO ()
printHashes = putStrLn hashes

starEnd :: String
starEnd = "                                                         *"

stars' :: String
stars' = "**********************************************************"

stars :: String
stars = "************************"

hashes :: String
hashes = "########################"

lines' :: String
lines' = "=============================================================================================="
