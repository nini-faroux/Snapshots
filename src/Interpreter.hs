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

loop :: Statement -> Interpreter () 
loop stmt = do
    liftIO $ putStrLn "next - inspect - stack"
    cmd <- liftIO getLine 
    case cmd of 
         "next"    -> execute stmt
         "inspect" -> displayEnv >> loop stmt
         "stack"   -> displayStack >> loop stmt
         _         -> (liftIO $ putStrLn "invalid command") >> loop stmt

execute :: Statement -> Interpreter () 
execute stmt@(Assign name exp) = do
    store stmt
    env <- gets pEnv 
    ins <- gets stack
    val <- runR exp 
    modify (\s -> s { pEnv = M.insert name val env })
    displayEnv
    displayStack

execute (Sequence s1 s2) = do
    loop s1
    loop s2

execute stmt@(If expr s1 s2) = do
    store stmt
    (B b) <- runR expr 
    store stmt
    if b then loop s1 else loop s2

execute stmt@(While expr s1) = do
    store stmt 
    (B b) <- runR expr 
    if b then (do loop s1; loop stmt) else return ()

execute stmt@(Print (Var name)) = do
    store stmt
    env <- gets pEnv 
    case lookupVar name env of 
        Nothing    -> liftIO $ putStrLn "Variable not found"
        (Just val) -> liftIO . putStrLn $ "Variable " ++ show name ++ " = " ++ show val

displayEnv :: Interpreter () 
displayEnv = do
    env <- gets pEnv 
    liftIO . print $ toList env

displayStack :: Interpreter ()
displayStack = do
    ins <- gets stack
    liftIO $ print ins
    
displayVar :: Name -> Val -> Interpreter () 
displayVar name val = liftIO . putStrLn $ "- " ++ "Value " ++ show val ++ " assigned to variable " ++ show name

store :: Statement -> Interpreter ()
store stmt = do
    stk <- gets stack
    cp <- gets sp
    modify (\s -> s { sp = cp + 1, stack = stk ++ [(Just cp, stmt)] })

runR :: Expr -> Interpreter Val
runR exp = do
    env <- gets pEnv
    case runEval (eval exp) env of
      Left errMsg -> evalError errMsg
      Right value -> return value

evalError :: String -> Interpreter a
evalError errMsg = do
    liftIO . putStrLn $ "- " ++ errMsg
    throwError (IError errMsg)
