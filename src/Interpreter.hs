module Interpreter where 

import Language
import Data.Map as M
import Control.Monad.Trans.State
import Control.Monad.Trans.Except 

type Evaluator a = StateT Env (ExceptT String IO) a

runEvaluator :: Evaluator a -> IO (Either String (a, Env))
runEvaluator e = runExceptT $ runStateT e M.empty


