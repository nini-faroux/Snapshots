module Evaluator where

import           Control.Monad.Except       (throwError)
import           Control.Monad.Identity
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import qualified Data.Map                   as M
import           Language

type Eval a = ReaderT Env (ExceptT String Identity) a

runEval :: ReaderT r (ExceptT String Identity) a -> r -> Either String a
runEval rex env = runIdentity . runExceptT $ runReaderT rex env

eval :: Expr -> Eval Val
eval (Const v)   = return v
eval (Add e0 e1) = evalInt (+) e0 e1
eval (Sub e0 e1) = evalInt (-) e0 e1
eval (Mul e0 e1) = evalInt (*) e0 e1
eval (Div e0 e1) = evalInt div e0 e1
eval (Not e)     = evalBool' not e
eval (Or e0 e1)  = evalBool (||) e0 e1
eval (And e0 e1) = evalBool (&&) e0 e1
eval (Eq e0 e1)  = evalComp (==) e0 e1
eval (Gt e0 e1)  = evalComp (>) e0 e1
eval (Lt e0 e1)  = evalComp (<) e0 e1
eval (Var n)     = do env <- ask; lookupVar n env

evalInt :: (Int -> Int -> Int) -> Expr -> Expr -> Eval Val
evalInt f e0 e1 = do
    x <- eval e0
    y <- eval e1
    case (x, y) of
        (I x, I y) -> return . I $ f x y
        _          -> throwError "Type error in arithmetic expression - expected type Int"

evalComp :: (Int -> Int -> Bool) -> Expr -> Expr -> Eval Val
evalComp f e0 e1 = do
    x <- eval e0
    y <- eval e1
    case (x, y) of
        (I x, I y) -> return . B $ f x y
        _          -> throwError "Type error in comparison expression - expected type Int"

evalBool :: (Bool -> Bool -> Bool) -> Expr -> Expr -> Eval Val
evalBool f e0 e1 = do
    x <- eval e0
    y <- eval e1
    case (x, y) of
        (B x, B y) -> return . B $ f x y
        _          -> throwError "Type error in boolean expression - expected type Bool"

evalBool' :: (Bool -> Bool) -> Expr -> Eval Val
evalBool' f e = do
    x <- eval e
    case x of
        B x -> return . B $ f x
        _   -> throwError "Type error in boolean expression - expected type Bool"

lookupVar :: Monad m => Name -> M.Map Name a -> m a
lookupVar n mp =
    case M.lookup n mp of
        Just x  -> return x
        Nothing -> fail ("Unknown Variable " ++ n)
