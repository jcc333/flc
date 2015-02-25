module Eval where
import Ast
import Lrt
import qualified Data.Set as Set
import qualified Data.Map as Map

type Tree = Lrt String

data Env = Env Tree (Map Tree (Set Tree))

-- on the fence about this
-- on one hand, we can just let the user do whatever and end up with a lot of Nils,
-- on the other, this might be overly-protective.
-- TODO: The solution, I guess, is actually to include `valid`, to determine validibility
-- alongside `assert`, which blid-fires, more or less, and `safeAssert`, to assert iff valid
-- Alternatively: See if implicature invalidates the whole tree, and if so, error. Else, go for it.
merge env@(Env et rs) t = 
  if valid t et
  then (Env (greatestLower et t) rs, Right None)
  else (env, Left "Cannot merge invalid trees without data loss\n")

type Result = Either String (Maybe Tree)

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

class Eval a where
  assert :: Env -> a -> (Env, Result) --appends the statement's encoding to the environment's lrt. if incompatible, this returns 'Nil', and leaves the environment unchanged
  --retract :: Env -> a -> (Env, Result) --retracts the statement's encoding from the environment's lrt
  --overwrite :: Env -> a -> (Env, Result) -- appends the statement's encoding to the environment's lrt. if incompatible, this overwrites the previous branch
  --query :: Env -> a -> (Env, Result) --tests whether or not the lrt models the proposition
  --
{-
// for a kb with no contents
?- assert Alice . sex : female & Bob . sex : male
?- Alice . sex //for simple queries in which a partial signature is given, we lazily iterate over the bag of results

  female

?- Alice.sex:female

  T

?- Alice.sex:male //F, since we can see that Alice has an exclusive edge to sex:female

  F

?- assert Alce.sex:male //F, since we can see that Alice has an exclusive edge to sex:female
  
  F

?- assert sex:male -> chromosomes.y : T

  T

?- Alice.chromosomes : y

  F

?- Bob.chromosomes.y

  Bob.chromosomes:y : T

?- Bob.chromosomes:y
 
  T

?- assert Alice.from.Texas //TODO: Add whitespace-insensitive identifiers (trim whitespace from either end)

?- assert Alice.from.Austin

?- assert Alice.from.6th street : False //creates an LRT terminating in Nil

?- all Alice . from //TODO: Add 'all' keyword

Texas : T
Austin : T
6th street : T
6th street : F

?- assert "http://thing.with.dots.in.text" . web addresss

  T

?- "http://thing.with.dots.in.text" . web addresss

  T

-} 

instance Eval Term where
  assert env term =
    let termTree = encode term
    in merge env termTree

instance Eval Conj where
  assert env conj = case conj of
    c -> merge env $ encode c
    HoistTerm t -> assert env t

instance Eval Exp where
  assert (Env t rules) exp = case exp of
    Arrow predicate consequent -> 
      let predicateTree = encode predicate
      let consequentTree = encode consequent
      let updated = Map.adjust (updateRule consequentTree) $ rules
      in (Env t updated, Right Nothing)
      where updateRule ct (pt, cts) = (pt, Set.insert cts ct)
    HoistConj c -> assert env c

