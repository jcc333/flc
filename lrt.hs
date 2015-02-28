module Lrt where
import Data.Monoid as Monoid
import Data.Maybe as Maybe
import Data.Map as Map

data Node a = Root -- used in the top level of the tree
            | Vertex a deriving (Eq, Show)

instance Functor Node where
  fmap f (Vertex a) = Vertex $ f a
  fmap f Root = Root

--instance (Monoid a) => Monoid Node a where
data Lrt a = Inc (Node a) (Map a Lrt a)
           | Exc (Node a) (Lrt a)
           | Nil
           deriving (Eq, Show)

vertex (Inc (Vertex a) _) = Just a
vertex (Exc (Vertex a) _) = Just a
vertex _ = Nothing

children (Inc _ bs) = bs
children (Exc _ b) = Map.singleton $ vertex b b
children _ = Map.empty

subgraph (Inc la lbs) (Inc ra rbs) = 
  la == ra &&
    all (\ l -> elem l rightVertices) leftVertices &&
      all (subgraphAny rbs) lbs
        where 
          leftVertices = catMaybes (map vertex lbs)
          rightVertices = catMaybes (map vertex rbs)
          subgraphAny ts t = any (\ r -> t `subgraph` r) ts
subgraph (Exc la lb) (Exc ra rb) = la == ra && lb `subgraph` rb
subgraph (Exc la lb) (Inc ra rbs) = la == ra && lb `elem` rbs
subgraph (Inc _ _) (Exc _ _) = False

greatestLower :: Lrt a -> Lrt a -> Lrt a
greatestLower lhs rhs = 
  case (lhs, rhs) of
    (Inc a lbs,  Inc a rbs) -> Inc a lbs ++ rbs
    (Inc la lbs, Inc ra rbs) ->
    (Exc la lb,  Inc ra rbs) -> 
    (Inc la lbs, Exc ra rb)  -> 
    (Exc la lb,  Exc ra rbs  -> 
  where lesserEdge (Inc a [b]) (Exc a b) = Just $ Exc a b
        lesserEdge (Exc a b) (Inc a [b]) = Just $ Exc a b
        lesserEdge (Exc a b) (Exc a b) = Just $ Exc a b
        lesserEdge _ _ = Nothing

leastUpper :: Lrt a -> Lrt a -> Lrt a
leastUpper lhs rhs = lhs

instance Eq a => Ord (Lrt a) where
  (<=) l r = r `subgraph` l

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
