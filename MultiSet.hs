module MultiSet where

data MSet a = MS [(a, Int)]
  deriving (Show)

-- represents an instance of an empty multiset, created using the value constructor above, I use it as a shortcut for "MS []" (and, of course, not for pattern matching since it doesn't ?????????), it also helps with memory usage 
empty :: MSet a 
empty = MS []

-- two Multisets are equal iff they share the same keys and the occurrences of each key are equal --> We can re-use subeq, two occurrences are equal iff one is <= than the other and viceversa 
instance Eq a => Eq (MSet a) where
  m1 == m2 = (subeq m1 m2) && (subeq m2 m1)



-- I chose to implement the foldr function
instance Foldable MSet where
  foldr f z (MS []) = z
  foldr f z (MS ((x,_):xs)) = f x (foldr f z (MS xs))


-- assuming the keys of the set are equateable, the following iterates over the first set checking that the occurrences of the first are at least the same as the second multiset's
subeq :: Eq a => MSet a -> MSet a -> Bool
subeq (MS []) _ = True --the empty set is always a subset of every other set
subeq (MS ((x,n):xs)) m2 = n <= occs m2 x && subeq (MS xs) m2 

-- takes as input a multiset with key type "a" and a key of type "a" and returns a new multiset containing the new element in the correct bag, with increased multiplicity
-- the constraint we impose on the type parameter is that it should also be an instance of the "Eq" typeclass, whereas the constraint "homogeneous type for all the keys" is implicit in the function declaration

add :: Eq a => MSet a -> a -> MSet a
add (MS []) x = MS [(x, 1)]
add (MS ((y,n):ys)) x
  | x == y = MS ((y,n+1):ys)
  | otherwise = let MS zs = add (MS ys) x in MS ((y,n):zs)


addMultiple :: Eq a => MSet a -> [a] -> MSet a
addMultiple (MS xs) [] = MS xs
addMultiple (MS []) (x:xs) = addMultiple (add empty x) xs
addMultiple (MS ((y,n):ys)) (x:xs) = add (addMultiple (MS ((y,n):ys)) xs) x

-- this is a utility function which I exploit to implement easily the 'union' function, since it's useful I've decided to export it anyways
addNTimes :: Eq a => MSet a -> a -> Int -> MSet a
addNTimes ms x n = addMultiple ms $ replicate n x

-- the approach here is to leave m2 constant while we reduce the first multiset to an empty one, recursively applying "add" to "x" : ERROR, LEAVES MULTIPLICITY BEHIND?
union :: Eq a => MSet a -> MSet a -> MSet a
union (MS []) m2 = m2
union (MS ((x,n):xs)) m2 = addNTimes (union (MS xs) m2) x n

-- counts the number of occurrences in the first parameter (multiset) of the second parameter (element), if the key is not there the function returns 0, same if the multiset is empty
occs :: Eq a => MSet a -> a -> Int
occs (MS []) _ = 0
occs (MS ((y,n):ys)) x
  | x == y = n
  | otherwise = occs (MS ys) x

-- counts the number of elements of the multisets
-- Note: this version could be very inefficient whenever the multiplicity of each key is a big number, because the operator "++" operates in linear time with respect to the length of the lists to concatenate
-- Depending on the dataset, a method that recursevely applies the head operator (:) could be a better solution
elems :: MSet a -> [a]
elems (MS []) = []
elems (MS ((x,n):xs)) = replicate n x ++ elems (MS xs) 



{-
"mapMSet" couldn't be a suitable implementation of fmap for because "mapMSet" has to return a "well formed" multiset
     that means that we have to manage collisions since we are mapping over the keys
        that means that we must assume that the keys are equatable (therefore introducing a class constraint over the type variable "b")
        (note: we didn't introduce a class constraint over the type variable "a" since "a" is part of what we assume to be a "well formed multiset" when we specify "MSet a", plus the method would work anyways)
            that means the method mapMSet couldn't possibly be the implementation of "fmap", since the signature of the former is more constrained than the latter's.
              fmap = mapMSet -- (a -> b) -> f a -> f b doesn't assume that "b" is equatable!
-}
mapMSet :: Eq b => (a -> b) -> MSet a -> MSet b
mapMSet f (MS []) = empty
mapMSet f (MS ((x,n):xs)) = let MS ys = mapMSet f (MS xs) in addNTimes (MS ys) (f x) n


-- given a list, it returns a new valid MSet containing the elements of the list 
createMSet :: Eq a => [a] -> MSet a
createMSet [] = empty
createMSet (x:xs) = addMultiple empty (x:xs) 


-- this method only checks whether two MultiSets have the same keys, we'll be usign the same paradigm as for subEq
shallowEquals :: Eq a => MSet a -> MSet a -> Bool 
shallowEquals m1 m2 = subeqKeys m1 m2 && subeqKeys m2 m1
  where subeqKeys (MS []) _ = True --the empty set is always a subset of every other set
        subeqKeys (MS ((x,_):xs)) m2 = 0 < occs m2 x && subeq (MS xs) m2 