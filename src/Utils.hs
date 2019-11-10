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

-- Interpreter Display helpers 

printStack :: (MonadIO m, Show a) => [a] -> m () 
printStack xs = printI stars >> foldr (\x -> (>>) (liftIO (print x >> newLine))) (return ()) xs >> printI stars 

displayVar :: MonadIO m => Name -> Val -> m ()
displayVar name val = printI $ "- " ++ "Value " ++ show val ++ " assigned to variable " ++ show name

printI :: (MonadIO m, Show a) => a -> m ()
printI xs = liftIO $ newLine >> print xs >> newLine

printExecuting :: MonadIO m => Statement -> m () 
printExecuting s = printI $ "Executing instruction: " ++ show s

printVariable :: MonadIO m => Name -> Val -> m () 
printVariable name val = printI $ "Variable " ++ name ++ " = " ++ show val

printNotFound :: MonadIO m => m () 
printNotFound = printI "Variable not found"

printAtStart :: MonadIO m => m () 
printAtStart = printI "Can't go back"

printInvalid :: MonadIO m => m () 
printInvalid = printI "Error: Invalid Command"

-- File loading helpers 

printStars :: IO () 
printStars = putStrLn stars 

newLine :: IO () 
newLine = putStrLn ""

defaultFile :: String
defaultFile = "./test/TestAll.test"

loadingFile :: IO ()
loadingFile = putStrLn "Loading Default file"

stars :: String
stars = "********************"

