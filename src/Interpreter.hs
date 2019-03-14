module Interpreter where 

import Language
import Evaluator
import Data.Map as M hiding (null)
import Data.Maybe (fromMaybe)
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

type Id = Int
type StackPtr = Int
type Instruction = (Id, Statement)
type Stack = [Instruction]
newtype IError = IError String deriving Show

data PState = 
    PState {
        pEnv :: Env
      , stack :: Stack
      , sp :: StackPtr
      , snapshots :: [(Int, Env)]
    } deriving Show

bootState :: PState 
bootState = PState { 
    pEnv = M.empty
  , stack = [] 
  , sp = 0
  , snapshots = []
  }

loop :: Statement -> Interpreter () 
loop stmt = do
    liftIO $ putStrLn "next - back - inspect - stack - snapshot - pointer"
    cmd <- liftIO getLine 
    case cmd of 
         "next"     -> step stmt
         "back"     -> back stmt
         "inspect"  -> displayEnv >> loop stmt
         "stack"    -> displayStack >> loop stmt
         "pointer"  -> displaySP >> loop stmt
         "snapshot" -> displaySS >> loop stmt
         _          -> liftIO (putStrLn "invalid command") >> loop stmt

back :: Statement -> Interpreter ()
back stmt = do
    cp <- gets sp 
    if cp < 2 
    then do 
       liftIO $ putStrLn "Can't go back"
       loop stmt 
    else reset stmt

reset :: Statement -> Interpreter () 
reset stmt = do
    jump
    cp <- gets sp
    stk <- gets stack
    resetEnv
    resetSS 
    modify (\s -> s { stack = init stk })
    let newStmt = fromMaybe stmt $ Prelude.lookup cp stk
    loop newStmt

jump :: Interpreter () 
jump = do
   cp <- gets sp
   let ptr' = cp - 2
   modify (\s -> s { sp = ptr' } )

resetEnv :: Interpreter () 
resetEnv = do
    cp <- gets sp
    ss <- gets snapshots
    let env' = fromMaybe M.empty $ Prelude.lookup cp ss
    modify (\s -> s { pEnv = env' } )

resetSS :: Interpreter ()
resetSS = do
    cp <- gets sp
    ss <- gets snapshots 
    let ss' = takeWhile (\x -> fst x <= cp) ss
    modify (\s -> s { snapshots = ss' })

execute :: Statement -> Interpreter () 
execute stmt@(Assign name exp) = do
    updateState stmt
    env <- gets pEnv 
    ins <- gets stack
    val <- runR exp 
    store stmt name val 

execute stmt@(Sequence s1 s2) = do
    updateState stmt
    loop s1
    loop s2

execute stmt@(If expr s1 s2) = do
    updateState stmt
    (B b) <- runR expr 
    if b then loop s1 else loop s2

execute stmt@(While expr s1) = do
    updateState stmt
    (B b) <- runR expr 
    if b then (do loop s1; loop stmt) else return ()

execute stmt@(Print (Var name)) = do
    updateState stmt
    env <- gets pEnv 
    case lookupVar name env of 
        Nothing    -> liftIO $ putStrLn "Variable not found"
        (Just val) -> liftIO . putStrLn $ "Variable " ++ show name ++ " = " ++ show val

step :: Statement -> Interpreter () 
step s1 = do
    liftIO . putStrLn $ "Executing instruction -> " ++ show s1
    execute s1

updateState :: Statement -> Interpreter ()
updateState stmt = do
    cp <- gets sp
    stk <- gets stack
    env <- gets pEnv
    ss <- gets snapshots 
    let ss' = ss ++ [(cp, env)]
    modify (\s -> s { sp = cp + 1, stack = stk ++ [(cp, stmt)], snapshots = ss' })

store :: Statement -> Name -> Val -> Interpreter ()
store stmt name val = do
    env <- gets pEnv
    modify (\s -> s { pEnv = M.insert name val env })
    cp <- gets sp
    ss <- gets snapshots
    env' <- gets pEnv
    let ss' = ss ++ [(cp, env')]
    modify (\s -> s { snapshots = ss' })

displaySP :: Interpreter () 
displaySP = do
    sp <- gets sp
    liftIO $ print sp

displaySS :: Interpreter () 
displaySS = do
    ss <- gets snapshots
    liftIO $ print ss

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
