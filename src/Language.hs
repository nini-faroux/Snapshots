module Language where 

import qualified Data.Text as T 

data Val = 
    I Int 
  | B Bool
  deriving (Eq, Show, Read)

data Expr = 
    Const Val 
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
  deriving (Eq, Show, Read)

data Statement = 
    Sequence Statement Statement 
  | While Expr Statement
  | If Expr Statement Statement 
  | Assign Name Expr 
  | Print Expr 
  deriving (Show, Eq, Read)

type Name = T.Text
