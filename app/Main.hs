{-|
Module      : LogLang
Description : A stupid experiment in logical calculation.
Copyright   : © 2021 Thibault Polge
License     : GPL-3
Maintainer  : thibault@thb.lt
Stability   : experimental

LogLang is an attempt at writing some sort of reducer for logical
expressions.
-}

module Main (Expr (..), reduce, main) where

import Data.Map as M
import Data.Set as S
import Text.Parsec.Expr
import Parser
import Types

reduce :: Expr -> Expr
-- Simplify ⊻, -> and <->
reduce (If a b) = reduce $ Not (And b (Not a))
reduce (Iff a b) = reduce $ Or (And a b) (And (Not a) (Not b)) -- By definition
reduce (Xor a b) = reduce $ (And (Or a b) (Not (And a b))) -- By definition.
reduce (Not F) = T
reduce (Not T) = F
reduce (Not (Not a)) = reduce a
reduce (Not a) = let a' = reduce a in
                   if a == a' then Not a' else reduce $ Not a' -- Avoid a cycle
reduce (And F a) = F
reduce (And a F) = F
reduce (And T a) = reduce a
reduce (And a T) = reduce a
reduce (Or _ T) = T
reduce (Or T _) = T
reduce (Or a F) = reduce a
reduce (Or F a) = reduce a
reduce (And a (Not b)) | a == b = F
reduce (And (Not a) b) | a == b = F
reduce (Or a (Not b)) | a == b = T
reduce (Or (Not a) b) | a == b = T
-- De Morgan
reduce (And (Not a) (Not b)) = reduce $ Not (Or a b)
reduce (Or (Not a) (Not b)) = reduce $ Not (And a b)
-- Base
reduce (And a b) = let a' = reduce a
                       b' = reduce b
                   in
                     if (a == a') && (b == b')
                     then And a' b'
                     else reduce $ And a' b'
reduce (Or a b) = let a' = reduce a
                      b' = reduce b
                   in
                     if (a == a') && (b == b')
                     then Or a' b'
                     else reduce $ Or a' b'
reduce a = a

-- | Walk an expression, optionally substituting parts of it or
-- collecting info.
walk :: Monoid a => (Expr -> a -> (Expr, a)) -> Expr -> Expr
walk = undefined


-- | Collect all free variables names in expr
variables :: Expr -> Set Char
variables = v S.empty
  where
    v :: Set Char -> Expr -> Set Char
    v s (Variable x) = S.insert x s
    v s (Not x) = v s x
    v s (And a b) = v2 s a b
    v s (Or a b) = v2 s a b
    v s (If a b) = v2 s a b
    v s (Iff a b) = v2 s a b
    v s (Xor a b) = v2 s a b
    v s T = s
    v s F = s
    v2 s a b = let s' = v s a
                   s'' = v s b
               in
                 S.union s' s''

-- | Substitute all occurences of a variable for another expression.
populate :: Expr -> Map Char Expr -> Expr
populate = undefined


main = putStrLn "Nothing yet."
