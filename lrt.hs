module Lrt where
import Data.List as List hiding (delete, isPrefixOf)
import Data.Monoid as Monoid
import Data.Maybe as Maybe
import Debug.Trace
import Text.Printf

data Node a = Root -- used in the top level of the tree
            | Vertex a deriving (Eq, Show)

instance Functor Node where
  fmap f (Vertex a) = Vertex $ f a
  fmap f Root = Root

data Lrt a = Inc (Node a) [Lrt a]
           | Exc (Node a) (Lrt a)
           | Nil
           deriving (Eq, Show)

node (Inc n _) = Just n
node (Exc n _) = Just n
node Nil = Nothing

vertex (Inc (Vertex a) _) = Just a
vertex (Exc (Vertex a) _) = Just a
vertex _ = Nothing

children (Inc _ bs) = bs
children (Exc _ b) = [b]
children _ = []

-- We define a partial ordering by specificity for propositional trees:
-- we say for two trees a and b, a <= b iff:
--    b is a subgraph of a
--    labels in a are at least as restrictive as in b
instance (Ord a) => Ord (Lrt a) where
  (<=) Nil _ = True
  (<=) (Inc la lbs) (Inc ra rbs) =
    la == ra && all (\ t -> isJust $ subgraphLookup t lbs) rbs
  (<=) (Exc la lb) (Exc ra rb) = la == ra && rb <= lb
  (<=) (Exc la lb) (Inc ra rbs) = la == ra && (isJust $ subgraphLookup lb rbs)
  (<=) (Inc _ _) (Exc _ _) = False

subgraphLookup t bs =
  let vertexEq v t = vertex t == Just v
  in do
    v <- vertex t
    b <- List.find (vertexEq v) bs
    if t <= b
      then Just b
      else Nothing

-- A couple of convenience functions used for higher level LRT operations
labelMatches lbs rbs =
  labelMatchesAux lbs rbs ([], [])
  where labelMatchesAux lbs rbs (matchAcc, unAcc) =
          case (lbs, rbs) of
            ([], []) ->   (matchAcc, unAcc)
            ([], rrem) -> (matchAcc, unAcc ++ rrem)
            (lrem, []) -> (matchAcc, unAcc ++ lrem)
            (lh:lt, rs) ->
              case branchMatch lh rs of
                Just (match, newRs) -> 
                  labelMatchesAux lt newRs ((lh, match) : matchAcc, unAcc)
                Nothing -> labelMatchesAux lt rs (matchAcc, lh:unAcc)

branchMatch t bs = branchMatchAux t bs [] 
  where branchMatchAux t [] _ = Nothing
        branchMatchAux t (bh:bt) seen = 
          if node t == node bh
          then Just (bh, seen ++ bt)
          else branchMatchAux t bt $ bh:seen

-- two trees are compatible automatically if they diverge at the top level:
--  they can't contradict one another because forall paths through them, their
--  signatures diverge at the toplevel node.
--  That is:
--  they don't have equal vertices here,
--  and the signatures include the vertices here,
--  so the signatures cannot be equal.
-- two trees are only incompatible if either:
--  1) exactly one of them is Nil
--  2) their children are incompatible
--  3) they diverge at the node under scrutiny:
--      in two exclusive nodes with matching vertices:
--        a) the signature ends here, no conflict
--        b) the signatures continue, but are compatible here
--           so we test the subtrees for incompatibilities
--        c) the signatures diverge here
compatible Nil Nil = True
compatible Nil _ = False
compatible (Inc la lbs) (Inc ra rbs) = 
  la /= ra || compatibleBranches
  where compatibleBranches =
          let (matches, _) = (labelMatches lbs rbs)
          in all (uncurry compatible) matches
compatible (Exc la lb) (Inc ra rbs) = 
  la /= ra || treesConverge
  where treesConverge = case rbs of 
          [] -> True
          [rb] -> node lb == node rb || compatible lb rb
          _ -> False
compatible (Exc la lb) (Exc ra rb) =
  la /= ra || (node lb == node rb && lb `compatible` rb)
compatible l r = r `compatible` l

-- Merging LRTs
-- greatestLower computes the greatest lower bound of two LRTs
-- as before, LRTs are ordered in generality, so asserting a new fact via
-- the greatestLower function computes the least specific LRT which attempts
-- to include all of the nodes from the left and right lrts
greatestLower l r =
  if r `compatible` r && all ((== Just Root) . node) [l, r]
  then gl l r
  else Nil
    where gl Nil _ = Nil
          gl (Inc l lbs) (Inc _ rbs)   = Inc l $ unmatches ++ merged
            where (matches, unmatches) = labelMatches lbs rbs
                  merged = map (\ (a, b) -> gl a b) matches
          gl (Exc l lb)   (Inc _ [rb]) = Exc l $ gl lb rb
          gl (Inc l [lb]) (Exc _ rb)   = Exc l $ gl lb rb
          gl (Exc l lb)   (Exc  _ rb)  = Exc l $ gl lb rb
          gl (Inc l [])   (Exc _ rb)   = Exc l rb

-- greatestLower is more useful in fact-merging, but leastUpper is present for
-- completeness, if nothing else
leastUpper l r = 
  if r `compatible` r && all ((== Just Root) . node) [l, r]
  then lu l r
  else Nil
    where lu Nil r = r
          lu (Inc l lbs) (Inc _ rbs) =
            let (matches, unmatches) = labelMatches lbs rbs
                merged = map (\ (a, b) -> lu a b) matches
            in Inc l $ merged
          lu (Inc l [lb]) (Exc _ rb)   = Inc l [lu lb rb]
          lu (Inc l [])   (Exc _ rb)   = Inc l []
          lu (Exc l lb)   (Inc _ [rb]) = Inc l [lu lb rb]
          lu (Exc l lb)   (Exc  _ rb)  = Exc l $ lu lb rb

instance Functor Lrt where
  fmap f Nil = Nil
  fmap f (Exc a b) = Exc (fmap f a) $ (fmap f) b
  fmap f (Inc a bs) = Inc (fmap f a) $ map (fmap f) bs


instance (Ord a, Show a) => Monoid (Lrt a) where
  mempty = Inc Root []
  mconcat = foldl mappend mempty
  mappend = greatestLower

childrenSat lrt bs = all (\ t -> any (\ c -> c `sat` t) $ children lrt) bs

sat Nil r = Nil == r
sat lrt (Inc Root bs) = childrenSat lrt bs
sat (Exc lv lb) (Inc rv bs) = lv == rv && case bs of
  [] -> True
  [rb] -> lb `sat` rb
  _ -> False
sat lrt (Inc vert bs) = node lrt == Just vert && childrenSat lrt bs
sat (Exc Root lb) (Exc Root rb) = lb `sat` rb
sat (Exc lv lb) (Exc rv rb) = lv == rv && lb `sat` rb
sat _ _ = False

(|=) model exp | model == Nil = True
               | otherwise = sat model exp

paths :: Lrt a -> [Lrt a]
paths Nil = [Nil]
paths (Inc n []) = [Inc n []]
paths (Inc n bs) = 
  let branchPaths = map paths bs
      flattened = concat branchPaths
  in map (\ p -> Inc n [p]) $ flattened
paths (Exc n b) = [Exc n p | p <- paths b]
    
isPrefixOf (Inc l [])   (Inc r _)    | l == r = True
isPrefixOf (Inc l [])   (Exc r _)    | l == r = True
isPrefixOf (Inc l [lb]) (Inc r [rb]) | l == r = lb `isPrefixOf` rb
isPrefixOf (Exc l lb)   (Exc r rb)   | l == r = lb `isPrefixOf` rb
isPrefixOf Nil Nil = True
isPrefixOf _ _ = False

-- deletion from an lrt
-- each lrt ends in terminals, T :: Inc (Node a) [] | Nil
-- deleting Nil-terminated branches requires deleting their dependent branches
requires l r | node l /= node r  = False
requires (Inc _ _)   (Inc _ [])  = True
requires (Inc _ _)   (Exc _ _)   = False
requires (Exc _ _)   (Inc _ _)   = False
requires (Exc _ lb)  (Exc _ rb)  = lb `requires` rb
requires (Inc _ lbs) (Inc _ rbs) =
  let (matches, unmatches) = labelMatches lbs rbs
  in matches /= [] && all (uncurry requires) matches

terminal path = case path of
  Inc _ [] -> path
  Inc _ [b] -> terminal b
  Exc _ b | terminal b == b -> path
  Exc n b -> terminal b
  Nil -> Nil

removeTerminal p =
  let t = terminal p
  in removeTerminal' p t
  where removeTerminal' p t | p == t = Nothing
        removeTerminal' p t = Just $ removeTerminal'' p t
        removeTerminal'' p t =
          case p of
            Inc n [b] | b == t -> Inc n []
            Inc n [b] -> Inc n [removeTerminal'' b t]
            Exc n b -> Exc n $ removeTerminal'' b t

delete env del =
  let ps = paths env
      (reqs, nonreqs) = partition (requires del) ps
      post = catMaybes $ map removeTerminal reqs
  in mconcat $ nonreqs ++ post
