module Ast where

data Term = Symbol String
          | Bang String Term
          | Dot String Term
          deriving Show

data Conj = HoistTerm Term
          | And Term Term
          deriving Show

data Exp = HoistConj Conj
         | Arrow Conj Conj
         deriving Show

data Stmt = Assert Exp
          | Retract Exp
          | Query Exp
          deriving Show
