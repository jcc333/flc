module Lrt where
import Data.List as List hiding (delete, isPrefixOf)
import Data.Monoid as Monoid
import Data.Maybe as Maybe
import Debug.Trace

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
--  1) one of them is Nil
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
--  'greatestLower' or 'leastUpper'
-- They assume that the two branches in question are compatible and correctly rooted
--  and that they share a label
--greatestLower l r | trace ("GREATEST LOWER called on:\n" ++ show l ++ "\n\nAND\n\n" ++ show r ++ "\n") False = undefined
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

instance (Ord a, Show a) => Monoid (Lrt a) where
  mempty = Inc Root []
  mconcat = foldl mappend mempty
  mappend = greatestLower

childrenSat lrt bs = all (\ t -> any (\ c -> c `sat` t) $ children lrt) bs

--sat l r | trace ("SAT called on:\n" ++ show l ++ "\n\nAND\n\n" ++ show r ++ "\n") False = undefined
sat Nil Nil = True
sat Nil _ = False
sat _ Nil = False
sat lrt (Inc Root []) = True
sat lrt (Inc Root bs) = childrenSat lrt bs
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

--this is a stupid way of implementing this; there should be a way to find matches and just, not copy them
delete l r =
  let rps = paths r
      lps = paths l
      dps = filter (\ lp -> not $ any (\ rp -> rp `isPrefixOf` lp) rps) lps
  in mconcat dps

a = (Inc Root 
      [
        Inc (Vertex 1) [
          Exc (Vertex 3)
            (Inc Root [])
        ]
      , Inc (Vertex 2) [
          Inc (Vertex 4) [
            Inc (Vertex 5) 
              [
                Inc (Vertex 6) []
              , Inc (Vertex 7) []
              ]

          ]
        ]
      ])

b = (Exc Root 
      (Inc (Vertex 1) [
        Inc (Vertex 2) [
          Exc (Vertex 3)
            (Inc (Vertex 4) 
            [ Inc (Vertex 5) []
            , Inc (Vertex 6) []
            , Inc (Vertex 7) []
            , Inc (Vertex 8) []
            ])
        ]
      ]))

c = (Exc Root 
      (Inc (Vertex 1) [
        Inc (Vertex 2) [
          Exc (Vertex 3)
            (Inc (Vertex 4) 
            [])
        ]
      ]))

d = (Exc Root 
      (Inc (Vertex 1) [
        Inc (Vertex 2) [
          Exc (Vertex 3)
            (Inc (Vertex 4) 
            [ Inc (Vertex 5) []
            , Inc (Vertex 6) []
            ])
        ]
      ]))

--find _ Nil = Nil
--find Nil _ = Nil
--find env (Inc Root []) = (Inc Root [])
--find env (Inc Root bs) = map (\ b -> (Inc Root [Lrt.find env b])) bs
--find env exc@(Exc Root _) = if env `sat` exc then exc else Nil

