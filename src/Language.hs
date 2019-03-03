module Language where 

import qualified Data.Text as T 
import qualified Data.Map as M 

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

type Env = M.Map Name Val

type Name = T.Text

data Val = 
    I Int 
  | B Bool
  deriving (Eq, Show, Read)
