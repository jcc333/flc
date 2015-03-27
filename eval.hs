module Eval where
import Ast
import Lrt
import qualified Data.Set as Set
import qualified Data.Map as Map

type Tree = Lrt String
type Map = Map.Map
type Set = Set.Set

data Env = { facts :: Tree, rules ::  (Map Tree (Set Tree)) }

eval :: Env -> Stmt -> (Env, Exp)
--eval takes an environment and a statement and returns the updated environment and the result of the statement (query result, success or failure of an assertion, etc.)
eval env (Assert exp) = case exp of
  HoistConj _ -> addFact (facts env) (encode exp)
  Arrow pred consq -> addRule (encode pred) (encode consq)
eval env (Query exp) = (facts env) `sat` (encode exp) --T/F for now, will add better query support later
eval env (All exp) = eval env (Query exp)
eval env (Retract exp) = removeFact env (encode exp)

class ELEncodable a where
  encode :: a -> Tree

class ELDecodable a where
  decode :: Tree -> [a]

instance ELEncodable Term where
  encode t = case t of
    Symbol "F" -> Nil
    Symbol "T" -> Root []
    Symbol s -> Inc s []
    Is s t -> Exc s (encode t)
    Dot s t -> Inc s [encode t]

instance ELDecodable Term where
  decode t = case t of
    Root [] -> [Symbol "T"]
    Nil -> [Symbol "F"]
    Exc s branch -> [Is s $ decode branch]
    Inc s branches -> [Dot s (decode b) | b <- branches]
    Root branches -> map decode branches

instance ELEncodable Conj where
  encode c = case c of
    And lt rt -> greatestLower (encode lt) (encode rt)
    HoistTerm t -> (encode t)
  
instance ELDecodable Conj where
  decode t = case t of
    Inc s [] -> map HoistTerm [Symbol s]
    Exc s branch -> map HoistTerm [Is s t]
    Inc s branches -> map HoistTerm [Dot s b | b <- branches]

eval env (Assert (Arrow pred cons)) =
  let predTree = encode pred
      consTree = encode cons
      consSet = Map.lookup predTree (rules env)
      postSet = Set.insert consTree consSet
  in (Env (facts env) Map.insert predTree postSet (rules env), Symbol "T")
eval env (Assert hoist) =
  let lrt = encode hoist
      pre = facts env
      post = Env (pre `mconcat` lrt) (rules env)
  in if post == Nil then (env, Nil) else (Env post (rules env), decode lrt)

eval env (Retract (Arrow pred cons)) =
  let predTree = encode pred
      consTree = encode cons
      consSet = Map.lookup predTree (rules env)
      postSet = Set.delete consTree consSet
  in (Env (facts env) Map.insert predTree postSet (rules env), Symbol "T")
eval env (Retract hoist) =
  (Env post (rules env), lrt)
  where lrt = encode hoist
        pre = facts env
        post = Env (pre `delete` lrt) (rules env) --TODO: define `delete`. it probably has something to do with `findAll`...

eval env (Query (Arrow pred _)) =
  let predTree = encode pred
      contains = Map.member predTree (rules env)
  in if contains
     then (env, Inc "T" [])
     else (env, Nil)
eval env (Query hoist) = 
  if (facts env) `sat` (encode hoist)
  then (env, Inc "T" [])
  else (env, Nil)
eval env (All exp) = (env, Inc "throw new UnsupportedOperationException(\"well this is awkward\");" [])

--find :: Lrt a -> Lrt a -> Lrt a 
find lrt (Symbol s) = --do we have this symbol as a child-node?
find lrt (Dot s t) = --do we have s as a child-node and can we find t from our s-correspondant?
find lrt (Is s t) = --do we have s as a child-node and can we find t from our s-correspondant?

--delete :: Lrt a -> Lrt a -> Lrt a
delete env term = --similar to find, but we keep a pointer to the tree we're keepingas an accumulator and then return that tree with the absence of the specified sub-tree?
