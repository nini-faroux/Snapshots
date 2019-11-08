module Interpreter where

import           Control.Monad              (void, when)
import           Control.Monad.Except       (throwError)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
import qualified Data.Map                   as Map
import           Data.Maybe                 (fromMaybe, isNothing)
import           System.Environment hiding  (setEnv)
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
type LookupStack = [(Statement, Id)]
newtype IError = IError String deriving Show

data PState =
    PState {
        pEnv        :: Env
      , iStack      :: IStack
      , vStack      :: VStack
      , lookupStack :: LookupStack
      , sp          :: Pointer
    } deriving Show

bootState :: PState
bootState = PState {
    pEnv = Map.empty
  , iStack = []
  , vStack = []
  , lookupStack = []
  , sp = 0
  }

loop :: Statement -> Interpreter ()
loop stmt = do
    printI ("At Instruction: " ++ show stmt)
    printI "(n, next) - (b, back) - (i, inspect) - (s, iStack) - (v, vStack) - (l, lookup) - (p, pointer)"
    cmd <- liftIO getLine
    case cmd of
         "n"  -> step stmt
         "b"  -> back stmt
         "i"  -> displayEnv    >> loop stmt
         "s"  -> displayIStack >> loop stmt
         "p"  -> displaySP     >> loop stmt
         "v"  -> displayVStack >> loop stmt
         "l"  -> displayLookup >> loop stmt 
         _    -> printInvalid  >> loop stmt

step :: Statement -> Interpreter ()
step s1 = do
    printI $ "Executing instruction: " ++ show s1
    execute s1

back :: Statement -> Interpreter () 
back stmt = do 
    cp <- gets sp 
    if cp < 1 
       then atStart stmt 
       else do 
         iStk <- gets iStack 
         lStk <- gets lookupStack 
         moveBack $ getParent stmt lStk iStk

moveBack :: (Id, Statement) -> Interpreter () 
moveBack (newSp, newStmt) = do 
    vStk <- gets vStack 
    iStk <- gets iStack 
    lStk <- gets lookupStack 
    let newVstk = setVstk newSp vStk 
    let newIstk = take newSp iStk 
    let newEnv  = setEnv newVstk 
    modify (\s -> s { sp = newSp, iStack = newIstk, vStack = newVstk, pEnv = newEnv })
    loop newStmt

getParent :: Statement -> LookupStack -> IStack -> (Id, Statement)
getParent s lStk iStk = (index, snd $ iStk !! index) 
    where 
      index = fromMaybe 0 (lookup s lStk)

setVstk :: Int -> VStack -> VStack
setVstk newSp vStk = 
    if newSp <= 1 
       then [] 
       else take newSp vStk

setEnv :: VStack -> Env 
setEnv vStk = 
    if null vStk 
       then Map.empty 
       else snd $ last vStk 

atStart :: Statement -> Interpreter () 
atStart s = printI "Can't Go Back" >> loop s

execute :: Statement -> Interpreter ()
execute stmt@(Assign name exp) = do
    val <- runR exp
    store stmt name val
    updateState stmt

execute stmt@(Sequence s1 s2) = do
    updateLookup s1 s2 
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
    when b $ do loop s1
                loop stmt

execute stmt@(Noop s1) = loop s1

execute stmt@(Print (Var name)) = do
    updateState stmt
    env <- gets pEnv
    case lookupVar name env of
        Nothing    -> printI "Variable not found"
        (Just val) -> printI $ "Variable " ++ name ++ " = " ++ show val

updateLookup :: Statement -> Statement -> Interpreter () 
updateLookup s1 s2 = do
    cp    <- gets sp 
    lStk  <- gets lookupStack
    modify (\s -> s { lookupStack = check cp s1 s2 lStk })
    where 
      check p s1 s2 l 
        | isNothing (lookup s1 l) = l ++ [(s1, p), (s2, p)]
        | otherwise               = l
       
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

displayLookup :: Interpreter () 
displayLookup = do 
    stk <- gets lookupStack 
    printStack stk

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
