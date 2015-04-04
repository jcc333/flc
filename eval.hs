module Eval where
import Ast
import Lrt
import Debug.Trace
import qualified Data.Set as Set
import qualified Data.Map as Map

type Tree = Lrt String
type Map = Map.Map
type Set = Set.Set

data Env = Env { facts :: Tree, rules :: Map Tree (Set Tree) }

emptyEnv = Env (Inc Root []) Map.empty

data Result = Result { env :: Env, result :: Either String [Exp] }

instance Show Result where
  show (Result env r) = either id (unlines . map display) r

class ELEncodable a where
  encode :: a -> Tree

class ELDecodable a where
  decode :: Tree -> [a]

instance ELEncodable Term where
  encode term = 
    let firstPass t = case t of
          Symbol "F" -> Nil
          Symbol "T" -> Inc Root []
          Symbol sym -> Inc (Vertex sym) []
          Is sym tail -> Exc (Vertex sym) (firstPass tail)
          Dot sym tail -> Inc (Vertex sym) [firstPass tail]
        termPass = firstPass term
    in case termPass of
        Inc Root [] -> Inc Root []
        Nil -> Nil
        _ -> Inc Root [termPass]

instance ELEncodable Conj where
  encode c = case c of
    And lt rt -> greatestLower (encode lt) (encode rt)
    HoistTerm t -> (encode t)


instance ELDecodable Term where
  decode t = case t of
    Inc Root [] -> [Symbol "T"]
    Nil -> [Symbol "F"]
    Exc s branch -> [Is (sym s) t | t <- decode branch]
    Inc s branches -> concat [[Dot (sym s) t | t <- decode b] | b <- branches]
    where sym n = case n of
            Root -> "T"
            Vertex s -> s
  
instance ELDecodable Conj where
  decode lrt = [HoistTerm term | term <- decode lrt]

instance ELDecodable Exp where
  decode lrt = [HoistConj conj | conj <- decode lrt]

eval env (Assert   exp@(HoistConj conj)) = assertFact env exp conj
eval env (Retract  exp@(HoistConj conj)) = retractFact env exp conj
eval env (Query    exp@(HoistConj conj)) = queryFact env exp conj
eval env (All      exp@(HoistConj conj)) = allFacts env exp conj
eval env (Assert  (Arrow pred cons)) = assertArrow env pred cons
eval env (Retract (Arrow pred cons)) = retractArrow env pred cons
eval env (Query   (Arrow pred cons)) = queryArrow env pred cons
eval env (All     (Arrow pred _)) = allArrows env pred

--Handling propositions
assertFact env exp conj =
  let lrt = encode conj
      pre = facts env
      post = greatestLower pre lrt
  in if pre `compatible` lrt
     then Result env { facts = post } $ Right [exp]
     else Result env $ Left "Did not assert contradiction"

retractFact env exp conj =
  let lrt = encode conj 
      pre = facts env
      post = pre `delete` lrt
  in if not $ pre == post
     then Result env { facts = post } $ Right [exp]
     else Result env $ Left "No expression to retract"

queryFact env exp conj =
  if (facts env) |= (encode conj)
  then Result env $ Right [trueExp]
  else Result env $ Right [falseExp]

allFacts env exp conj =
  let found = find (facts env) (encode conj)
      exps = found >>= decode
      result = case exps of
        [] -> Left "No matches found"
        _ -> Right exps
  in Result env result
  where
    find lhs rhs =
      if lhs |= rhs
      then filter (\ p -> rhs `isPrefixOf` p) (paths lhs)
      else []

--Handling inference rules
assertArrow env pred cons =
  let predTree = encode pred
      consTree = encode cons
      consSet = case Map.lookup predTree (rules env) of
        Just set -> set
        Nothing -> Set.empty
      postConsSet = Set.insert consTree consSet
      postRules = Map.insert predTree postConsSet (rules env)
      postEnv = env { rules = postRules }
  in Result postEnv $ Left "Asserted inference rule"

retractArrow env pred cons =
  let predTree = encode pred
      consTree = encode cons
      consSet = case Map.lookup predTree (rules env) of
        Just set -> set
        Nothing -> Set.empty
      postSet = Set.delete consTree consSet
      postRules = Map.insert predTree postSet (rules env)
      postEnv = env { rules = postRules }
  in Result postEnv $ Left "Retracted inference rule"

queryArrow env pred cons =
  let predTree = encode pred
      consTree = encode cons
      result = case Map.lookup predTree (rules env) of
        Just set -> 
          let consList = Set.toList set
              consConjs = concat $ map decode consList
          in Right [Arrow pred c | c <- consConjs]
        Nothing -> Left "No matching inference rule"
  in Result env result

allArrows env pred =
  let predTree = encode pred
      result = case Map.lookup predTree (rules env) of
        Just set -> 
          let consList = Set.toList set 
              consConjs = consList >>= decode
          in Right [Arrow pred c | c <- consConjs]
        Nothing -> Left "No matching inference rule"
  in Result env result

falseExp = HoistConj $ HoistTerm $ Symbol "F"
trueExp = HoistConj $ HoistTerm $ Symbol "T"

