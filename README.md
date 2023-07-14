# haskell-multisets
This academic project demonstrates the usage of Haskell's most important constructs, including the IO Monad, through the implementation of a Multiset data structure.

The assignment requires to implement a type constructor providing the functionalities of multisets (also known as bags), that is, collections of elements where the order does not count, but each element can occur several 
times. 

The implementation must be based on the following concrete Haskell definition of the MSet type constructor:

    data MSet a = MS [(a, Int)]
        deriving (Show)

    
Therefore an MSet contains a list of pairs whose first component is an element of the multiset, and the second component is its multiplicity, that is the number of occurrences of such element in the multiset.

An MSet is well-formed if for each of its pairs (v,n) it holds n > 0, and if it does not contain two pairs (v,n) and (v',n') such that v = v'. 
