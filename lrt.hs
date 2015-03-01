module Lrt where
import Data.List as List
import Data.Monoid as Monoid
import Data.Maybe as Maybe

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
-- examples:
--   YES: 0 . [1 2 3] <= 0 . [1 2] : there's a branch in b for each branch in a
--   YES: 0 ! 1 <= 0 . [1 2] : the above, plus, a is more specific
--   NO:  0 . 1 <= 0 ! 1 : b is more restrictive than a
--   NO:  0 . [1 2] <= 0 . [1 2 3 4 5] : b is more specified than a
--   YES: F <= 0 ! 1 : F is the least LRT
--   YES: 0 . [(some large set of propositions)] <= T : T is greatest LRT
instance (Ord a) => Ord (Lrt a) where
  (<=) Nil _ = True
  (<=) (Inc la lbs) (Inc ra rbs) =
    la == ra && all (\ t -> isJust $ subgraphLookup t lbs) rbs
  (<=) (Exc la lb) (Exc ra rb) = la == ra && rb <= lb
  (<=) (Exc la lb) (Inc ra rbs) = la == ra && (isJust $ subgraphLookup lb rbs)
  (<=) (Inc _ _) (Exc _ _) = False

subgraphLookup t bs = 
  vertex t >>= \ v ->
    List.find (vertexEq v) bs >>= \ b ->
      if t <= b
      then Just b
      else Nothing
  where vertexEq v t = vertex t == Just v

-- two trees are compatible automatically if they diverge at the top level:
--  they can't interfere with one another because forall vertices later, their
--  signatures concretely diverge at the nodes in question. That is, they don't
--  have equal vertices here, and the signatures include the vertices here, so
--  the signatures cannot be equal.
-- two trees are only incompatible if either:
--  1) one or them is Nil
--  2) their children are incompatible
--  3) they diverge at the node:
--      in two exclusive nodes with matching vertices:
--        a) the signature ends here, no conflict
--        b) the signatures continue, but are compatible here
--           so we test the subtrees for incompatibilities
--        c) the signatures diverge here
compatible Nil Nil = True
compatible Nil _ = False
compatible (Inc la lbs) (Inc ra rbs) = 
  la /= ra || all (\ t -> compAux t rbs) lbs
  where compAux t bs = case t `subgraphLookup` bs of
          Just match -> t `compatible` match
          Nothing -> True
compatible (Exc la lb) (Inc ra rbs) = la /= ra || treesConverge
  where treesConverge = case rbs of 
          [] -> True
          [rb] -> node lb == node rb || compatible lb rb
          _ -> False
compatible (Exc la lb) (Exc ra rb) = la /= ra || lb `compatible` rb
compatible l r = r `compatible` l

-- These merge functions are not guaranteed correct unless called from 
--  'merge' or 'greatestLower'.
-- They assume that the two branches in question are compatible and correctly rooted
--  and that they share a label
greatestLower l r =
  if r `compatible` r && all ((== Just Root) . node) [l, r]
  then glMerge l r
  else Nil

glMerge Nil _ = Nil
glMerge (Inc l lbs) (Inc _ rbs) =
  let (matches, unmatches) = labelMatches lbs rbs
      merged = map (\ (a, b) -> glMerge a b) matches
  in Inc l $ unmatches ++ merged
glMerge (Exc l lb) (Inc _ [rb]) = Exc l $ glMerge lb rb
glMerge (Inc l [lb]) (Exc _ rb) = Exc l $ glMerge lb rb
glMerge (Exc l lb) (Exc  _ rb) = Exc l $ glMerge lb rb
glMerge (Inc l []) (Exc _ rb) = Exc l rb

leastUpper l r = 
  if r `compatible` r && all ((== Just Root) . node) [l, r]
  then luMerge l r
  else Nil

luMerge Nil r = r
luMerge (Inc l lbs) (Inc _ rbs) =
  let (matches, unmatches) = labelMatches lbs rbs
      merged = map (\ (a, b) -> luMerge a b) matches
  in Inc l $ merged
luMerge (Exc l lb) (Inc _ [rb]) = Inc l $ [luMerge lb rb]
luMerge (Inc l [lb]) (Exc _ rb) = Inc l $ [luMerge lb rb]
luMerge (Exc l lb) (Exc  _ rb) = Exc l $ luMerge lb rb
luMerge (Inc l []) (Exc _ rb) = Inc l []


labelMatches lbs rbs =
  labelMatchesAux lbs rbs ([], [])
  where labelMatchesAux lbs rbs (matchAcc, unAcc) =
          case (lbs, rbs) of
            ([], []) -> (matchAcc, unAcc)
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


instance Functor Lrt where
  fmap f Nil = Nil
  fmap f (Exc a b) = Exc (fmap f a) $ (fmap f) b
  fmap f (Inc a bs) = Inc (fmap f a) $ map (fmap f) bs

instance (Ord a) => Monoid (Lrt a) where
  mempty = Inc Root []
  mconcat = foldl mappend mempty
  mappend = greatestLower
  {-
instance Monad Lrt where
  (>>=) (Inc Root bs) f = flatten $ map (fmap f) bs
  (>>=) (Inc (Vertex a) bs) f = mconcat $ (f a):(map (fmap f) (children a))
  (>>=) (Exc Root b) f = Exc Root $ f b 
  (>>=) (Exc (Vertex a) b) f = mappend (f a) (fmap f b)
  (>>=) Nil f = Nil
  return a = Inc (Vertex a) []-}
