module Interpreter where 

import Language
import Evaluator
import Data.Map as M
import System.Environment 
import Control.Monad (void)
import Control.Monad.Trans.State
import Control.Monad.Trans.Except 
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)

type Interpreter a = StateT PState (ExceptT IError IO) a

runInterpreter :: Interpreter a -> IO (Either IError (a, PState))
runInterpreter e = runExceptT $ runStateT e bootState

runI :: Statement -> IO ()
runI statement = void . runInterpreter $ execute statement

type Id = Maybe Int
type StackPtr = Int
type Instruction = (Id, Statement)
type Stack = [Instruction]
newtype IError = IError String deriving Show

data PState = 
    PState {
        pEnv :: Env
      , stack :: Stack
      , sp :: StackPtr
    } deriving Show

bootState :: PState 
bootState = PState { 
    pEnv = M.empty
  , stack = [] 
  , sp = 0
  }

execute :: Statement -> Interpreter () 
execute stmt@(Assign name exp) = do
    store stmt
    env <- gets pEnv 
    ins <- gets stack
    val <- runR exp 
    modify (\s -> s { pEnv = M.insert name val env })
    displayI name val

execute (Sequence s1 s2) = do
    execute s1
    execute s2

execute stmt@(If expr s1 s2) = do
    store stmt
    (B b) <- runR expr 
    store stmt
    if b then execute s1 else execute s2

execute (Print (Var name)) = do
    env <- gets pEnv 
    case lookupVar name env of 
        Nothing    -> liftIO $ putStrLn "Variable not found"
        (Just val) -> liftIO . putStrLn $ "Variable " ++ show name ++ " = " ++ show val
    
displayI :: Name -> Val -> Interpreter () 
displayI name val = do
    env <- gets pEnv 
    ins <- gets stack 
    liftIO . print $ toList env
    liftIO $ print ins 
    liftIO . putStrLn $ "- " ++ "Value " ++ show val ++ " assigned to variable " ++ show name

store :: Statement -> Interpreter ()
store stmt = do
    stk <- gets stack
    cp <- gets sp
    modify (\s -> s { sp = cp + 1, stack = stk ++ [(Just cp, stmt)] })

runR :: Expr -> Interpreter Val
runR exp = do
    env <- gets pEnv
    case runEval (eval exp) env of
      Left errMsg -> notFound errMsg
      Right value -> return value

notFound :: String -> Interpreter a
notFound errMsg = do
    liftIO . putStrLn $ "- " ++ errMsg
    throwError (IError errMsg)
