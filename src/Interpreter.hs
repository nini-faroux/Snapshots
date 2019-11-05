module Interpreter where

import           Control.Monad              (void, when)
import           Control.Monad.Except       (throwError)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
import qualified Data.Map                   as Map
import           Data.Maybe                 (fromMaybe)
import           System.Environment
import           Evaluator
import           Language

type Interpreter a = StateT PState (ExceptT IError IO) a

runInterpreter :: Interpreter a -> IO (Either IError (a, PState))
runInterpreter interpreter = runExceptT $ runStateT interpreter bootState

runI :: Statement -> IO ()
runI statement = void . runInterpreter $ loop statement

type Id = Int
type Pointer = Int
type Instruction = (Id, Statement)
type IStack = [Instruction]
type VStack = [(Id, Env)]
type SeqStack = [Id]
newtype IError = IError String deriving Show

data PState =
    PState {
        pEnv      :: Env
      , iStack    :: IStack
      , vStack    :: VStack
      , seqStack  :: SeqStack
      , sp        :: Pointer
      , mode      :: Mode 
    } deriving Show

bootState :: PState
bootState = PState {
    pEnv = Map.empty
  , iStack = []
  , vStack = []
  , seqStack = []
  , sp = 0
  , mode = Normal
  }

data Mode = Normal | Loop deriving (Eq, Show)

loop :: Statement -> Interpreter ()
loop stmt = do
    printI "(n, next) - (b, back) - (i, inspect) - (s, iStack) - (v, vStack) - (a, seqStack) - (p, pointer)"
    cmd <- liftIO getLine
    case cmd of
         "n"  -> step stmt
         "b"  -> back stmt
         "i"  -> displayEnv    >> loop stmt
         "s"  -> displayIStack >> loop stmt
         "a"  -> displaySStack >> loop stmt
         "p"  -> displaySP     >> loop stmt
         "v"  -> displayVStack >> loop stmt
         "m"  -> displayMode   >> loop stmt 
         _    -> printInvalid  >> loop stmt

step :: Statement -> Interpreter ()
step s1 = do
    printI $ "Executing instruction: " ++ show s1
    execute s1

back :: Statement -> Interpreter ()
back stmt = do
    cp   <- gets sp
    sStk <- gets seqStack 
    if cp < 2 then atStart stmt  
              else if cp == 2 || length sStk == 1 
                   then resetStart else moveBack

atStart :: Statement -> Interpreter () 
atStart s = printI "Can't Go Back" >> loop s

resetStart :: Interpreter () 
resetStart = do 
    istk <- gets iStack 
    modify (\s -> s { sp = 0, iStack = [], seqStack = [], vStack = [], pEnv = Map.empty, mode = Normal })
    loop . snd $ head istk 

moveBack :: Interpreter () 
moveBack = do
    sp'  <- gets sp 
    vStk <- gets vStack 
    sStk <- gets seqStack 
    iStk <- gets iStack 
    let newSp   = last sStk
    let newVstk = take newSp vStk 
    let newIstk = take newSp iStk 
    let newEnv  = snd $ last newVstk 
    let newSstk = init sStk
    let newStmt = snd $ iStk !! last sStk
    modify (\s -> s { sp = newSp, iStack = newIstk, vStack = newVstk, seqStack = newSstk, pEnv = newEnv })
    loop . snd $ iStk !! last sStk 

execute :: Statement -> Interpreter ()
execute stmt@(Assign name exp) = do
    val <- runR exp
    store stmt name val
    updateState stmt

execute stmt@(Sequence s1 s2) = do
    updateSequence 
    updateState stmt
    loop s1
    loop s2

execute stmt@(If expr s1 s2) = do
    updateState stmt
    (B b) <- runR expr
    if b then loop s1 else loop s2

execute stmt@(While expr s1) = do
    updateState stmt
    setWhileMode 
    (B b) <- runR expr
    when b $ do loop s1
                loop stmt
    setNormalMode 

execute stmt@(Noop s1) = loop s1

execute stmt@(Print (Var name)) = do
    updateState stmt
    env <- gets pEnv
    case lookupVar name env of
        Nothing    -> printI "Variable not found"
        (Just val) -> printI $ "Variable " ++ name ++ " = " ++ show val

updateSequence :: Interpreter () 
updateSequence = do
    mode' <- gets mode 
    if mode' == Loop 
       then return ()  
       else do cp  <- gets sp 
               stk <- gets seqStack 
               modify (\s -> s { seqStack = stk ++ [cp] })

setWhileMode :: Interpreter () 
setWhileMode = modify (\s -> s { mode = Loop })

setNormalMode :: Interpreter () 
setNormalMode = modify (\s -> s { mode = Normal })

updateState :: Statement -> Interpreter ()
updateState stmt = do
    cp   <- gets sp
    istk <- gets iStack
    vstk <- gets vStack 
    env  <- gets pEnv 
    modify (\s -> s { sp = cp + 1, iStack = istk ++ [(cp, stmt)], vStack = vstk ++ [(cp, env)] })

store :: Statement -> Name -> Val -> Interpreter ()
store stmt name val = do
    env  <- gets pEnv
    let newEnv = Map.insert name val env
    modify (\s -> s { pEnv = newEnv })

displaySP :: Interpreter ()
displaySP = do
    sp <- gets sp
    printI sp

displayEnv :: Interpreter ()
displayEnv = do
    env <- gets pEnv
    printI $ Map.toList env

displayIStack :: Interpreter ()
displayIStack = do
    ins <- gets iStack
    printStack ins

displayVStack :: Interpreter () 
displayVStack = do 
    stk <- gets vStack 
    printStack stk

displaySStack :: Interpreter () 
displaySStack = do 
    stk <- gets seqStack 
    printI stk

displayMode :: Interpreter () 
displayMode = do 
    mode' <- gets mode 
    printI mode' 

printStack :: Show a => [a] -> Interpreter () 
printStack = foldr (\x -> (>>) (liftIO (print x >> putStrLn ""))) (return ()) 

displayVar :: Name -> Val -> Interpreter ()
displayVar name val = printI $ "- " ++ "Value " ++ show val ++ " assigned to variable " ++ show name

runR :: Expr -> Interpreter Val
runR expr = do
    env <- gets pEnv
    case runEval (eval expr) env of
      Left errMsg -> evalError errMsg
      Right value -> return value

evalError :: String -> Interpreter a
evalError errMsg = do
    printI errMsg
    throwError (IError errMsg)

printI :: Show a => a -> Interpreter ()
printI xs = liftIO $ putStrLn "" >> print xs >> putStrLn ""

printInvalid :: Interpreter () 
printInvalid = printI "Error: Invalid Command" 
