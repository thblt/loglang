module Types where

data Expr = T
          | F
          | If Expr Expr
          | Iff Expr Expr
          | Not Expr
          | And Expr Expr
          | Or Expr Expr
          | Xor Expr Expr
          | Variable Char
  deriving (Show, Eq)
