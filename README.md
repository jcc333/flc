Exclusion Logic in Haskell
==========================

- Exclusion logic (EL) is cool
- I'd like to create a fast, flexible distributed database for hierarchical metadata
- Exclusion logic uses labeled rooted trees (LRTs)
- LRTs are kind of difficult to program around on a good day
- They extend Rose Trees by labeling edges as well as vertices
- In addition, they impose restrictions based upon the semantics of edges based upon the edge-label
- for instance, in EL we can state that "A is B", or we can state that "A is exclusively B"
- (a.b, a!b)
- EL translates at the LRT level by labeling whatever alphabet of symbols at the vertices, and labeling the edges with the binary operator that connected the propositions
- A.B becomes a graph `A -(.)-> B`
- A!B becomes a graph `A -(!)-> B`
- And the type system from the LRT informs us that we cannot insert additional properties to A's "signature".
- By "signature", we mean the path from The tree's root to A.
- That said, we can still add "b.c":
- A!B.C becomes a graph `A -(!)-> B -(.)-> C`
- Now, dealing with a larger LRT composed of multiple assertions like that, we get vertices which are connected to nothing (leaves), those which are linked via `!` (exclusive nodes), and those which are linked via `.`.
- In haskell, it's easy to represent the collections of edges with lists from the inclusive node to its neighbors, and as a single neighbor for exclusive nodes
- This is just meant to be an in-memory reference implementation for testing purposes later on: I don't aim to be fast, and I don't aim to be clever here; just straightforward as possible
