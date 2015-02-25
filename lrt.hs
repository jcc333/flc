module Lrt where
import Data.Monoid as Monoid
import Data.Maybe as Maybe

data Lrt a = Root [Lrt a]
           | Inc a [Lrt a]
           | Exc a (Lrt a)
           | Nil
           deriving (Eq, Show)

vertex (Inc a _) = Just a
vertex (Exc a _) = Just a
vertex Nil = Nothing

children (Inc a bs) = bs
children (Exc a b) = [b]
children _ = Nil

childVertices (Inc _ bs) = catMaybes (map vertex bs)
childVertices (Exc _ b) = catMaybes [vertex b]
childVertices Nil = []

subgraph Nil _ = False
subgraph (Inc la lbs) (Inc ra rbs) = 
  la == ra &&
    all (\ l -> elem l rightVertices) leftVertices &&
      all (subgraphAny rbs) lbs
        where 
          leftVertices = catMaybes (map vertex lbs)
          rightVertices = catMaybes (map vertex rbs)
          subgraphAny ts t = any (\ r -> t `subgraph` r) ts
subgraph (Exc la lb) (Exc ra rb) = la == ra && lb `subgraph` rb
subgraph (Exc la lb) (Inc ra rbs) = la == ra
subgraph (Inc _ _) (Exc _ _) = False

leastUpper :: Lrt a -> Lrt a -> Lrt a
greatestLower :: Monoid a => Lrt a -> Lrt a -> Lrt a

instance Eq a => Ord (Lrt a) where
  (<=) l r = r `subgraph` l

instance Functor Lrt where
  fmap f Nil = Nil
  fmap f (Exc a b) = Exc (f a) (fmap f b)
  fmap f (Inc a bs) = Inc (f a) (map (fmap f) bs)

--ideally, we could provide a function from merges to annonymous monoid instances
instance Monoid Lrt where
  mempty = Nil
  mconcat ts = foldl greatestLower ts
  mappend l r = greatestLower l r

--implicitly, all Lrts of type a have a common root via type-compatibility.
--the trees, we can infer, belong in the same class of propositions because 
--the propositions that they encode lie in the same class necessarily
instance Monad Lrt where
  -- we can think of (>>=) for LRTs as applying a rule of inference to the database
  -- this works especially well when f :: a -> LRT a
  -- in that case, it's plain to see that f is really just a schema for implicature in the universe of exclusive logic over some proposition type a.
  -- by then calling mconcat, we als get to see a constuctive proof that f's application is coherent
  -- that is, `tree >>= f` is the application of `f` to `tree` as implicature or suggestion, where it is coherent.
  (>>=) :: (Inc a bs) f = mconcat (f a):(map f children a)
  (>>=) :: (Exc a b) f = mconcat (f a):(map f bs)
  (>>=) :: Nil f = Nil
  return a = Inc a []

