module Fundamentals where

data FreeFunctor f a where
    FreeFunctor :: f a -> (a -> b) -> FreeFunctor f b

instance Functor (FreeFunctor f) where
    -- fmap :: (a -> b) -> f a -> f b
    fmap fn (FreeFunctor fA fnAToB) = FreeFunctor fA (fn . fnAToB)


performTheFreeFunctorBruh :: (forall a. (a -> b) -> f a -> f b) -> FreeFunctor f b -> f b
performTheFreeFunctorBruh fmapF (FreeFunctor fA fnAB) = fmapF fnAB fA

class Monoid' a where
    append :: a -> a -> a
    empty :: a

instance Monoid' [a] where
    append a a' = a ++ a'
    empty = []

performFreeMonoidBruh :: (b -> b -> b) -> b -> [b] -> b
performFreeMonoidBruh = foldr

data MyThing a = MyThing a

-- append [Sum 1, Sum 3] [Sum 2] = [Sum 1, Sum 3, Sum 2]


-- Monads are a Tree with a's at the leaves.
-- You can grow new trees at the leaves.
-- bind takes a leaf and you grow a new tree at that leaf.

-- Instance of Monad this for Identity
-- and a performFreeMonadBruh.
-- Stick in Maybe and then List

data Free f a
    = Leaf a
    | Tree (f (Free f a))
