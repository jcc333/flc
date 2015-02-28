module Lrt where
import Data.Monoid as Monoid
import Data.Maybe as Maybe
import qualified Data.Map as Map

type Map k v = Map.Map k v

data Node a = Root -- used in the top level of the tree
            | Vertex a deriving (Eq, Show)

instance Functor Node where
  fmap f (Vertex a) = Vertex $ f a
  fmap f Root = Root

data Lrt a = Inc (Node a) (Map a (Lrt a))
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
children (Exc _ b) = case vertex b of
  Just v -> Map.singleton v b
  Nothing -> Map.empty
children _ = Map.empty

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
    la == ra && all (\ (_, t) -> isJust $ t `subgraphLookup` lbs) (Map.toList rbs)
  (<=) (Exc la lb) (Exc ra rb) = la == ra && rb <= lb
  (<=) (Exc la lb) (Inc ra rbs) = la == ra && (isJust $ lb `subgraphLookup` rbs)
  (<=) (Inc _ _) (Exc _ _) = False

subgraphLookup t bs = 
  vertex t >>= \ v ->
    Map.lookup v bs >>= \ b ->
      if t <= b
      then Just b
      else Nothing

greatestLower :: Lrt a -> Lrt a -> Lrt a
greatestLower lhs rhs = lhs
  {-case (lhs, rhs) of
    (Inc a lbs,  Inc a rbs) -> Inc a lbs ++ rbs
    (Inc la lbs, Inc ra rbs) -> 
    (Exc la lb,  Inc ra rbs) -> 
    (Inc la lbs, Exc ra rb)  -> 
    (Exc la lb,  Exc ra rbs  -> 
  where lesserEdge (Inc a [b]) (Exc a b) = Just $ Exc a b
        lesserEdge (Exc a b) (Inc a [b]) = Just $ Exc a b
        lesserEdge (Exc a b) (Exc a b) = Just $ Exc a b
        lesserEdge _ _ = Nothing-}

leastUpper :: Lrt a -> Lrt a -> Lrt a
leastUpper lhs rhs = lhs

instance Functor Lrt where
  fmap f Nil = Nil
  fmap f (Exc a b) = Exc (fmap f a) (fmap f b)
  fmap f (Inc a bs) = Inc (fmap f a) (map (fmap f) bs)

instance Monoid (Lrt a) where
  mempty = Inc Root []
  mconcat = foldl mappend mempty
  mappend = greatestLower
  {-
instance Monad Lrt where
  (>>=) (Inc Root bs) f = mconcat $ map (fmap f) bs
  (>>=) (Inc (Vertex a) bs) f = mconcat $ (f a):(map (fmap f) (children a))
  (>>=) (Exc Root b) f = Root f b 
  (>>=) (Exc (Vertex a) b) f = mappend (f a) (fmap f b)
  (>>=) Nil f = Nil
  return a = Inc (Vertex a) []-}
