module Language
    ( Expr(..)
    , Statement(..)
    , Env
    , Name
    , Val(..)
    ) where

import qualified Data.Map as Map

data Expr =
    Const Val
  | Var Name
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | And Expr Expr
  | Or Expr Expr
  | Gt Expr Expr
  | Lt Expr Expr
  | Eq Expr Expr
  | Not Expr
  deriving (Eq, Read)

data Statement =
    Sequence Statement Statement
  | While Expr Statement
  | If Expr Statement Statement
  | Assign Name Expr
  | Print Expr
  | Noop Statement
  deriving (Eq, Read)

type Env  = Map.Map Name Val
type Name = String

data Val =
    I Int
  | B Bool
  deriving (Eq, Show, Read)

instance Show Statement where 
  show (Sequence s1 s2) = " (Sequence " ++ show s1 ++ show s2 ++ ")"
  show (Assign name e1) = " (Assign " ++ name ++ " " ++ show e1 ++ ")" 
  show (While e1 s1)    = " (While " ++ show e1 ++ show s1 ++ ")"
  show (If e1 s1 s2)    = " (If " ++ show e1 ++ show s2 ++ show s2 ++ ")" 
  show (Print e1)       = " (Print " ++ show e1 ++ ")"

instance Show Expr where 
  show (Const val) = "(Const " ++ show val ++ ")"
  show (Var name)  = "(Var " ++ name ++ ") "
  show (Add e1 e2) = "(Add " ++ show e1 ++ show e2 ++ ")" 
  show (Sub e1 e2) = "(Sub " ++ show e1 ++ show e2 ++ ")"
  show (Mul e1 e2) = "(Mul " ++ show e1 ++ show e2 ++ ")"
  show (Div e1 e2) = "(Div " ++ show e1 ++ show e2 ++ ")"
  show (And e1 e2) = "(And " ++ show e1 ++ show e2 ++ ")"
  show (Or e1 e2)  = "(Or " ++ show e1 ++ show e2 ++ ")"
  show (Gt e1 e2)  = "(Gt " ++ show e1 ++ show e2 ++ ")"
  show (Lt e1 e2)  = "(Lt " ++ show e1 ++ show e2 ++ ")"
  show (Eq e1 e2)  = "(Eq " ++ show e1 ++ show e2 ++ ")"
  show (Not e1)    = "(Not " ++ show e1 ++ ")"
