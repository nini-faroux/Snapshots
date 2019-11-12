module Interpreter (runI) where

import           Control.Monad              (void, when)
import           Control.Monad.Except       (throwError)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
import qualified Data.Map                   as Map
import           Data.Maybe                 (fromMaybe, isNothing)
import           System.Environment hiding  (setEnv)
import           Evaluator                  (runEval, eval, lookupVar)
import           Language                   (Statement(..), Expr(..), Val(..), Env, Name)
import           Utils

-- | Monad for program evaluation 
type Interpreter a = StateT ProgramState (ExceptT IError IO) a

runInterpreter :: Interpreter a -> IO (Either IError (a, ProgramState))
runInterpreter interpreter = runExceptT $ runStateT interpreter bootState

runI :: Statement -> IO ()
runI statement = void . runInterpreter $ loop statement

-- | State types and type synonyms

-- | PC for moving forward and back
-- Increments on each 'next' evaluation
type ProgramCounter = Int

-- | Keeps track of the value of the PC
-- at each Statement that has been evaluated
type Position = Int

-- | Pair representing the Statement evaluated
-- at a particular position
type Instruction = (Position, Statement)

-- | Map from Statement to Position for easy look up
-- Map type guarantees no duplicate keys
type StatementPosition = Map.Map Statement Position

-- | List to keep track of evaluated Statements
type IStack = [Instruction]

-- | VS for keeping track of the variable
-- environment at each stage of evaluation
type VariableStack = [(Position, Env)]

-- | ES is for keeping track of the second Statement executed
-- in a Sequence Statement, as these are not evaluated immediately
type ExecutionStack = [Statement]

-- | The program state type to be passed around
-- Contains the above data structures, the Env and the PC
data ProgramState =
    ProgramState {
        programEnv        :: Env
      , iStack            :: IStack
      , variableStack     :: VariableStack
      , executionStack    :: ExecutionStack
      , statementPosition :: StatementPosition
      , programCounter    :: ProgramCounter
    } deriving Show

-- | Set initial values for state
bootState :: ProgramState
bootState = ProgramState {
    programEnv = Map.empty
  , iStack = []
  , variableStack = []
  , executionStack = []
  , statementPosition = Map.empty
  , programCounter = 0
  }

-- | Main options for the user
-- Accepts and executes valid commands
loop :: Statement -> Interpreter ()
loop stmt = do
  mainDisplay
  printAtInstruction stmt
  cmd <- getCommand
  case cmd of
       "n"  -> step stmt
       "b"  -> back stmt
       "i"  -> displayEnv    >> waitLoop stmt
       "s"  -> displayIStack >> waitLoop stmt
       "v"  -> displayVStack >> waitLoop stmt
       "q"  -> quit
       _    -> printInvalid  >> waitLoop stmt

-- | Display some program state requested by user
-- <e> brings user back to main options
waitLoop :: Statement -> Interpreter ()
waitLoop stmt = do
  displayWaitLoop
  cmd <- getCommand
  case cmd of
      "e" -> loop stmt
      _   -> printInvalid >> waitLoop stmt

-- | Execute "async" statement if it exists in execution stack
-- Rejects any potential "loose" statements at the end of the program, that might remain
-- from an async call, from before the user chose to move backwards in the execution
-- As these statements will have been removed from ES on step backwards 
asyncLoop :: Statement -> Interpreter ()
asyncLoop stmt = do
  es <- gets executionStack
  if stmt `notElem` es
     then return ()
     else popES stmt >> loop stmt

-- | Remove the Statement from the ES after evaluation
popES :: Statement -> Interpreter ()
popES stmt = do
  es <- gets executionStack
  modify (\s -> s { executionStack = filter (/= stmt) es })

-- | Execute the next statement
step :: Statement -> Interpreter ()
step s1 = do
  printExecuting s1
  execute s1

-- | Step backwards in the execution
-- Unless already at the start of the program
-- Always move back to the "Parent" Statement
-- Would otherwise be possible to lose connection to the next 
-- Sequence Statement in the program and incorrectly terminate early
back :: Statement -> Interpreter ()
back stmt = do
  pc <- gets programCounter
  if pc < 1
     then atStart stmt
     else do
       iStk <- gets iStack
       sPos <- gets statementPosition
       moveBack $ getParent stmt sPos iStk

-- | Sets the program counter to pointer value at the parent statement
-- And modifies the state accordingly
moveBack :: (Position, Statement) -> Interpreter ()
moveBack (newPc, newStmt) = do
  pc   <- gets programCounter
  vStk <- gets variableStack
  iStk <- gets iStack
  let newVstk = setVstk newPc vStk
  let newIstk = take newPc iStk
  modify (\s -> s { programCounter = newPc, iStack = newIstk, variableStack = newVstk, programEnv = setEnv newVstk })
  loop newStmt

-- | A statement's parent is the Sequence statement from which it was evaluated
-- Example - (Sequence (Assign "x" ...) (Assign "y" ...))
-- Here the Sequence Statement is parent to both Assign Statments
-- Parent's position is stored in the StatementPosition Map
getParent :: Statement -> StatementPosition -> IStack -> (Position, Statement)
getParent s sPos iStk = (position, snd $ iStk !! position)
  where
    position = fromMaybe 0 (Map.lookup s sPos)

-- | Removes given statement from ES
resetExecutionStack :: Statement -> Interpreter ()
resetExecutionStack stmt = do
  es <- gets executionStack
  if null es
     then return ()
     else modify (\s -> s { executionStack = filter (/= stmt) es })

-- | Resets variable stack on move backwards
setVstk :: Int -> VariableStack -> VariableStack
setVstk newPc vStk =
  if newPc <= 1
     then []
     else take newPc vStk

-- | Resets Variable environment on move backwards
setEnv :: VariableStack -> Env
setEnv vStk =
  if null vStk
     then Map.empty
     else snd $ last vStk

-- | Reject attempt to move back from start
atStart :: Statement -> Interpreter ()
atStart s = printAtStart >> loop s

-- | Statment execution functions
execute :: Statement -> Interpreter ()
execute stmt@(Assign name exp) = do
  val <- runR exp
  resetExecutionStack stmt
  store stmt name val
  updateState stmt

-- | Use asyncLoop for the second statement evaluation
-- Avoids "loose" statements executing at end of the program
-- when a user has moved back at one or more points during evaluation
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

-- | Add the two new statements of a Sequence statement with the position 
-- (PC) of the Sequence (parent) statement to the StatementPosition Map
updateSPos :: Statement -> Statement -> Interpreter ()
updateSPos s1 s2 = do
  pc    <- gets programCounter
  sPos  <- gets statementPosition
  modify (\s -> s { statementPosition = check pc s1 s2 sPos })
  where
    check p s1 s2 sPos
      | isNothing (Map.lookup s1 sPos) = mergeMap sPos (Map.fromList [(s1, p), (s2, p)])
      | otherwise                      = sPos

-- | Combine two Maps into one
mergeMap :: (Ord k, Num v) => Map.Map k v -> Map.Map k v -> Map.Map k v
mergeMap = Map.unionWith (+)

-- | Modify program's state
updateState :: Statement -> Interpreter ()
updateState stmt = do
  pc   <- gets programCounter
  istk <- gets iStack
  vstk <- gets variableStack
  env  <- gets programEnv
  modify (\s -> s { programCounter = pc + 1, iStack = istk ++ [(pc, stmt)], variableStack = vstk ++ [(pc, env)] })

-- | Add a new async statement to the ES
pushExecutionStack :: Statement -> Interpreter ()
pushExecutionStack stmt = do
  es <- gets executionStack
  if stmt `elem` es
     then return ()
     else modify (\s -> s { executionStack = es ++ [stmt] })

-- | Store a new variable and its value in environment Map
store :: Statement -> Name -> Val -> Interpreter ()
store stmt name val = do
  env  <- gets programEnv
  modify (\s -> s { programEnv = Map.insert name val env })

-- | Evaluate expression in the Reader Monad
-- Fails with an error if the expression is invalid
runR :: Expr -> Interpreter Val
runR expr = do
  env <- gets programEnv
  case runEval (eval expr) env of
    Left errMsg -> evalError errMsg
    Right value -> return value

-- | Quit the program
quit :: Interpreter ()
quit = void quitting

-- | Functions for displaying some aspect of
-- the program's current state
displayWaitLoop :: Interpreter ()
displayWaitLoop = printWaitLoop

displayEnv :: Interpreter ()
displayEnv = do
  env <- gets programEnv
  printEnv env

displayIStack :: Interpreter ()
displayIStack = do
  ins <- gets iStack
  printStack ins

displayVStack :: Interpreter ()
displayVStack = do
  stk <- gets variableStack
  printVHist stk
