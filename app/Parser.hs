module Parser (parser) where

import Text.Parsec.Expr
import Text.Parsec
import Text.Parsec.Token
import Types

parser = parse expr

expr :: Parsec String () Expr
expr = buildExpressionParser table term
  <?> "expression"

term = (fmap Variable lower)
  <?> "Atom"
  -- <|> parens expr

table   = [ [Infix   (char '∧' >> return And) AssocLeft
            , Infix   (char '∨' >> return Or) AssocLeft
            , Infix   (char '⊻' >> return Xor) AssocLeft]
          , [Infix   (char '→' >> return If) AssocLeft]
          , [Infix   (char '↔' >> return Iff) AssocLeft]
          , [prefix $ char '¬' >> return Not]]

prefix p = Prefix . chainl1 p $ return (.)
