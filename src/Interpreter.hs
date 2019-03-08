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

type Interpreter a = StateT PState (ExceptT String IO) a

runInterpreter :: Interpreter a -> IO (Either String (a, PState))
runInterpreter e = runExceptT $ runStateT e bootState

runI :: Statement -> IO ()
runI statement = void . runInterpreter $ execute statement

type Instruction = (Statement, Either NotFound (Name, Val))
type Instructions = [Instruction]
data NotFound = NotFound deriving Show

data PState = 
    PState {
        pEnv :: Env, 
        instructs :: Instructions
    } deriving Show

bootState :: PState 
bootState = PState { pEnv = M.empty, instructs = [] }

execute :: Statement -> Interpreter () 
execute stmt@(Assign name exp) = do
    store stmt $ Right name
    env <- gets pEnv 
    val <- runR exp 
    modify (\s -> s { pEnv = M.insert name val env })
    liftIO . putStrLn $ "- " ++ "Value " ++ show val ++ " assigned to variable " ++ show name

execute (Sequence s1 s2) = do
    execute s1
    execute s2

store :: Statement -> Either NotFound Name -> Interpreter ()
store stmt varName = do
   prevIns <- gets instructs
   case varName of
     Left NotFound -> storeVal stmt prevIns (Left NotFound)
     Right name -> do
       env <- gets pEnv
       case lookupVar name env of
         Nothing    -> storeVal stmt prevIns (Left NotFound)
         Just value -> storeVal stmt prevIns (Right (name, value))

storeVal :: Statement -> Instructions -> Either NotFound (Name, Val) -> Interpreter ()
storeVal st ins nmVal = 
  case nmVal of 
    Left NotFound -> updateIns $ ins ++ [(st, Left NotFound)]
    Right tup     -> updateIns $ ins ++ [(st, Right tup)]
  where updateIns ins = modify (\s -> s { instructs = ins })

runR :: Expr -> Interpreter Val
runR exp = do
    env <- gets pEnv
    case runEval (eval exp) env of
      Left errMsg -> notFound errMsg
      Right value -> return value

notFound :: String -> Interpreter a
notFound errMsg = do
    liftIO . putStrLn $ "- " ++ errMsg
    throwError errMsg
