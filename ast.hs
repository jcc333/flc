module Ast where

data Term = Symbol String
          | Dot String Term
          | Is String Term
          deriving (Eq, Show)

data Conj = HoistTerm Term
          | And Term Term
          deriving (Eq, Show)

data Exp = HoistConj Conj
         | Arrow Conj Conj
         deriving (Eq, Show)

data Stmt = Assert Exp
          | Retract Exp
          | Query Exp
          | All Exp
          deriving Show
