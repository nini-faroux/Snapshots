module Interpreter where 

import Language
import Data.Map as M
import Control.Monad.Trans.State
import Control.Monad.Trans.Except 

type Evaluator a = StateT PState (ExceptT String IO) a

runEvaluator :: Evaluator a -> IO (Either String (a, PState))
runEvaluator e = runExceptT $ runStateT e bootState

type Instruction = (Statement, Either String (Name, Val))
type Instructions = [Instruction]

data PState = 
    PState {
        pEnv :: Env, 
        instructs  :: Instructions
    } deriving Show

bootState :: PState 
bootState = PState { pEnv = M.empty,  instructs = [] }
