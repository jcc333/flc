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

class Display a where
  display :: a -> String

instance Display Term where
  display t = case t of
    Symbol s -> s
    Dot s t -> s ++ "." ++ display t
    Is s t -> s ++ ":" ++ display t

instance Display Conj where
  display c = case c of
    HoistTerm t -> display t
    And l r -> display l ++ " & " ++ display r

instance Display Exp where
  display e = case e of
    HoistConj c -> display c
    Arrow p c -> display p ++ " -> " ++ display c

instance Display Stmt where
  display s = case s of
    Assert e -> "assert" ++ stmtExp e
    Retract e -> "retract" ++ stmtExp e
    Query e -> "query" ++ stmtExp e
    All e -> "all" ++ stmtExp e

stmtExp e = " " ++ display e
