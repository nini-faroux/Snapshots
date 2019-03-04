module Evaluator where 

import Language 
import Control.Monad.Identity
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader 
import qualified Data.Map as M

type Eval a = ReaderT Env (ExceptT String Identity) a

runEval :: ReaderT Env (ExceptT String Identity) Val -> Env -> Either String Val
runEval rex env = runIdentity . runExceptT $ runReaderT rex env

eval :: Expr -> Eval Val
eval (Const v) = return v
eval (Add e0 e1) = evalInt (+) e0 e1 
eval (Sub e0 e1) = evalInt (-) e0 e1 
eval (Mul e0 e1) = evalInt (*) e0 e1
eval (Div e0 e1) = evalInt div e0 e1 

evalInt :: (Int -> Int -> Int) -> Expr -> Expr -> Eval Val
evalInt f e0 e1 = do
    x <- eval e0 
    y <- eval e1 
    case (x, y) of 
        (I x, I y) -> return . I $ f x y
        _          -> fail "Type error in arithmetic expression - expected type Int"
