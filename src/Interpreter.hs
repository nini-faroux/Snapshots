module Interpreter (runI) where

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
import           Utils

type Interpreter a = StateT ProgramState (ExceptT IError IO) a

runInterpreter :: Interpreter a -> IO (Either IError (a, ProgramState))
runInterpreter interpreter = runExceptT $ runStateT interpreter bootState

runI :: Statement -> IO ()
runI statement = void . runInterpreter $ loop statement

type ProgramCounter = Int
type Position = Int
type Instruction = (Position, Statement)
type StatementPosition = Map.Map Statement Position 

type IStack = [Instruction]
type VariableStack = [(Position, Env)]
type ExecutionStack = [Statement] 

data ProgramState =
    ProgramState {
        programEnv        :: Env
      , iStack            :: IStack
      , variableStack     :: VariableStack
      , executionStack    :: ExecutionStack 
      , statementPosition :: StatementPosition
      , programCounter    :: ProgramCounter
    } deriving Show

bootState :: ProgramState
bootState = ProgramState {
    programEnv = Map.empty
  , iStack = []
  , variableStack = []
  , executionStack = [] 
  , statementPosition = Map.empty
  , programCounter = 0
  }

loop :: Statement -> Interpreter ()
loop stmt = do
    mainDisplay
    printAtInstruction stmt
    cmd <- liftIO getLine
    case cmd of
         "n"  -> step stmt
         "b"  -> back stmt
         "i"  -> displayEnv    >> loop stmt
         "s"  -> displayIStack >> loop stmt
         "v"  -> displayVStack >> loop stmt
         _    -> printInvalid  >> loop stmt

asyncLoop :: Statement -> Interpreter () 
asyncLoop stmt = do 
    es <- gets executionStack 
    if stmt `notElem` es 
       then return ()
       else popES stmt >> loop stmt 

popES :: Statement -> Interpreter () 
popES stmt = do 
    es <- gets executionStack 
    modify (\s -> s { executionStack = filter (/= stmt) es })

step :: Statement -> Interpreter ()
step s1 = do
    printExecuting s1
    execute s1

back :: Statement -> Interpreter () 
back stmt = do 
    pc <- gets programCounter 
    if pc < 1 
       then atStart stmt 
       else do 
         iStk <- gets iStack 
         sPos <- gets statementPosition 
         moveBack $ getParent stmt sPos iStk

moveBack :: (Position, Statement) -> Interpreter () 
moveBack (newPc, newStmt) = do 
    pc   <- gets programCounter 
    vStk <- gets variableStack 
    iStk <- gets iStack 
    let newVstk = setVstk newPc vStk 
    let newIstk = take newPc iStk 
    modify (\s -> s { programCounter = newPc, iStack = newIstk, variableStack = newVstk, programEnv = setEnv newVstk })
    loop newStmt

resetExecutionStack :: Statement -> Interpreter () 
resetExecutionStack stmt = do 
    es <- gets executionStack 
    if null es 
       then return () 
       else modify (\s -> s { executionStack = filter (/= stmt) es })

getParent :: Statement -> StatementPosition -> IStack -> (Position, Statement)
getParent s sPos iStk = (position, snd $ iStk !! position) 
    where 
      position = fromMaybe 0 (Map.lookup s sPos)

setVstk :: Int -> VariableStack -> VariableStack
setVstk newPc vStk = 
    if newPc <= 1 
       then [] 
       else take newPc vStk

setEnv :: VariableStack -> Env 
setEnv vStk = 
    if null vStk 
       then Map.empty 
       else snd $ last vStk 

atStart :: Statement -> Interpreter () 
atStart s = printAtStart >> loop s

execute :: Statement -> Interpreter ()
execute stmt@(Assign name exp) = do
    val <- runR exp
    resetExecutionStack stmt
    store stmt name val
    updateState stmt

execute stmt@(Sequence s1 s2) = do
    updateSPos s1 s2 
    updateState stmt
    pushExecutionStack s2
    loop s1
    asyncLoop s2

execute stmt@(If expr s1 s2) = do
    updateState stmt
    (B b) <- runR expr
    if b then loop s1 else loop s2

execute stmt@(While expr s1) = do
    updateState stmt
    resetExecutionStack stmt 
    (B b) <- runR expr
    when b $ do loop s1
                loop stmt

execute stmt@(Noop s1) = loop s1

execute stmt@(Print (Var name)) = do
    updateState stmt
    resetExecutionStack stmt
    env <- gets programEnv
    case lookupVar name env of
        Nothing    -> printNotFound 
        (Just val) -> printVariable name val

updateSPos :: Statement -> Statement -> Interpreter () 
updateSPos s1 s2 = do
    pc    <- gets programCounter 
    sPos  <- gets statementPosition
    modify (\s -> s { statementPosition = check pc s1 s2 sPos })
    where 
      check p s1 s2 sPos 
        | isNothing (Map.lookup s1 sPos) = mergeMap sPos (Map.fromList [(s1, p), (s2, p)])
        | otherwise                      = sPos

mergeMap :: (Ord k, Num v) => Map.Map k v -> Map.Map k v -> Map.Map k v 
mergeMap = Map.unionWith (+)
       
updateState :: Statement -> Interpreter ()
updateState stmt = do
    pc   <- gets programCounter
    istk <- gets iStack
    vstk <- gets variableStack 
    env  <- gets programEnv 
    modify (\s -> s { programCounter = pc + 1, iStack = istk ++ [(pc, stmt)], variableStack = vstk ++ [(pc, env)] })

pushExecutionStack :: Statement -> Interpreter () 
pushExecutionStack stmt = do
    es <- gets executionStack 
    if stmt `elem` es 
       then return () 
       else modify (\s -> s { executionStack = es ++ [stmt] })

store :: Statement -> Name -> Val -> Interpreter ()
store stmt name val = do
    env  <- gets programEnv
    modify (\s -> s { programEnv = Map.insert name val env })

runR :: Expr -> Interpreter Val
runR expr = do
    env <- gets programEnv
    case runEval (eval expr) env of
      Left errMsg -> evalError errMsg
      Right value -> return value

displayPC :: Interpreter ()
displayPC = do
    pc <- gets programCounter
    printI pc

displayEnv :: Interpreter ()
displayEnv = do
    env <- gets programEnv
    printI $ Map.toList env

displayIStack :: Interpreter ()
displayIStack = do
    ins <- gets iStack
    printStack ins

displayVStack :: Interpreter () 
displayVStack = do 
    stk <- gets variableStack 
    printStack stk
