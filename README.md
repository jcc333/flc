Exclusion Logic in Haskell
==========================

- Exclusion logic is an alternative deontic logic Richard Evans at Maxis has written about
- It relies on strategically impoverishing the operators in the language to attain some performance benefits and computability while attempting to minimize the loss of expressivity
- Namely, it does away with 'or' and the 'not' we're used to
- Slightly more subtly, EL also restricts the class of inference rules we can create by syntactically eliminating recursive inference rules as invalid (no ` a -> (b -> c)` forward chain rules)
- In return, it adds two operators 'such that', and 'is only' (`.` and `:` in our case)
- Exclusion logic (EL) is cool
- I'd like to create a fast, flexible distributed database for hierarchical metadata
- Exclusion logic uses labeled rooted trees (LRTs)
- LRTs are kind of difficult to program around on a good day
- They extend Rose Trees by labeling edges as well as vertices
- In addition, they impose restrictions based upon the semantics of edges based upon the edge-label
- for instance, in EL we can state that "A is B", or we can state that "A is exclusively B"
- (a.b, a:b)
- EL translates at the LRT level by labeling whatever alphabet of symbols at the vertices, and labeling the edges with the binary operator that connected the propositions
- A.B becomes a graph from a to a list of destinations populated only by b
- A:B becomes a graph with one edge to b and one edge only
- And the type system from the LRT informs us that we cannot insert additional properties to the path from the tree's root to A.
- That said, we can still add "b.c":
- A:B.C becomes a graph `A -(:)-> B -[(.)-> C]`
- Now, dealing with a larger LRT composed of multiple assertions like that, we get vertices which are connected to nothing (leaves), those which are linked via `:` (exclusive nodes), and those which are linked via `.`.
- In haskell, it's easy to represent the collections of edges with lists from the inclusive node to its neighbors, and as a single neighbor for exclusive nodes
- This is just meant to be an in-memory reference implementation for testing purposes later on and pedagogical purposes now: I don't aim to be fast, and I don't aim to be clever here; just as straightforward as possible so that later I can understand what I'm doing when I'm confused as all get-out, trying to convert LRTs into B+ trees in c++ or something.

Example session of FLC
======================
// starting with a kb with no contents

?- assert Alice . sex : female & Bob . sex : male

//now the tree looks something like: T -.> Alice -.> sex -:> female
//                                     -.> Bob   -.> sex -:> male

?- Alice . sex

  female

//for simple queries in which a partial signature is given, we iterate over the bag of results

?- Alice.sex:female

  T

?- Alice.sex:male
  F
//F, since we can see that Alice has an exclusive edge to sex:female

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

?- assert Alice.from:Austin

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
