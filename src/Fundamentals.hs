module Fundamentals where

data CoYoneda f a where
    CoYoneda :: f a -> (a -> b) -> CoYoneda f b

instance Functor (CoYoneda f) where
    -- fmap :: (a -> b) -> f a -> f b
    fmap fn (CoYoneda fA fnAToB) = CoYoneda fA (fn . fnAToB)

performTheFreeFunctorBruh :: (forall a. (a -> b) -> f a -> f b) -> CoYoneda f b -> f b
performTheFreeFunctorBruh fmapy (CoYoneda fOfA aToB) = fmapy aToB fOfA

class Monoid' a where
    append :: a -> a -> a
    empty :: a

instance Monoid' [a] where
    append a a' = a ++ a'
    empty = []

performFreeMonoidBruh :: (b -> b -> b) -> b -> [b] -> b
performFreeMonoidBruh = foldr

-- append [Sum 1, Sum 3] [Sum 2] = [Sum 1, Sum 3, Sum 2]


-- Monads are a Tree with a's at the leaves.
-- You can grow new trees at the leaves.
-- bind takes a leaf and you grow a new tree at that leaf.

-- Instance of Monad this for Identity
-- and a performFreeMonadBruh.
-- Stick in Maybe and then List
data Free f a
    = Pure a -- Pure :: a -> m a
    | Free (f (Free f a)) -- Free :: f (m a) -> m a

class Monad' (m :: * -> *) where
    pure :: a -> m a
    bind :: (a -> m b) -> m a ->  m b

-- instance Monad' (Free []) where
--     pure = Pure

--     bind (Pure ls) f = f ls
--     bind (Free l:ls) f = flip bind f _

data Huh a
    = Some a
    | Nada

-- instance Functor Huh where
--     -- fmap :: (a -> b) -> Huh a -> Huh b
--     fmap f (Some a) = Some (f a)
--     fmap _ _ = Nada

instance Monad' (Free Huh) where
    pure = Pure
    bind f (Pure x) = f x
    bind f (Free (Some a)) = Free $ Some ((bind f) a)
    bind _ (Free Nada) = Free Nada


instance Monad' (Free []) where
    pure = Pure
    bind f (Pure x) = f x
    bind f (Free x) = Free $ fmap (bind f) x

-- newtype May a =
--     May { huh :: forall b. b -> (a -> b) -> b }

newtype Identity a = Identity { runId :: a } deriving (Functor)

instance Monad' (Free Identity) where
    pure = Pure
    bind f = \case
        Pure a -> f a
        Free a -> Free $ fmap (bind f) a


-- runFreeIdentity :: () -> Free f a -> f a
-- runFreeIdentity free
