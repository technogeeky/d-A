module Control.Applicative.I where

import Data.Functor.Identity (Identity(..))
import Data.Functor.Constant (Constant(..))
import Data.Monoid           (Monoid(..)  )
import Control.Monad (ap)
-- |
----------------------------------------------------------------------------------------------------------------------------------------
-- I      Weak combinatory model
----------------------------------------------------------------------------------------------------------------------------------------
class Functor a => I a where            -- has the same kind as Functor, namely (* -> *)      -> Constraint
     i :: a (d -> d)                                   -- djinn: f a     = a              -- id
     b :: a (     (c -> r) -> (d -> c) -> (d -> r))    -- djinn: f a b c = a  (b c)       -- (.)
     c :: a ((d -> c -> r) ->       c  -> (d -> r))    -- djinn: f a b c = a c b          -- flip
     s :: a ((d -> c -> r) -> (d -> c) -> (d -> r))    -- djinn: f a b c = a c (b c)      -- ap

     pure               :: v -> a v
     (|\/|)             :: v -> a v
     (|--|>|)           :: a (l -> r) -> (a l -> a r)
     (<*>)              :: a (l -> r) -> (a l -> a r)

     pure        = (|\/|)
     (<*>)       = (|--|>|)

     (<$>)       :: (x -> y) -> a x -> a y
     (<$>) f a   = fmap f a
{-
   law0_0             :: a r -> a r
   law0_1             :: a r -> a r

   law0_0 u = ( (|\/|) id )
                 |--|>| u
   law0_1 u = liftA (id) u
-}
   -- A has four Laws:
   -- (0) A   Identity   :   pure iId         <*> u           = u
   -- (1) A   Composition:   pure (.)         <*> u <*> v <*> w = u <*> (<*> v w)
   -- (2) The Homomprhism:   pure f <*> pure x                = pure (f x)
   -- (3) The Interchange:                       u <*> pure x = pure (<*> x) <*> u


-- Moufang Loop Identities
class Loop d c r where
     id1_l :: ((r -> d) -> r) -> c
     id1_r :: ((r -> d -> r)  -> c)

     id2_l :: ((d -> r) -> c) -> r           
     id2_r :: (d -> (r -> c -> r))           

     id3_l :: (r -> (  d -> c )) -> r        -- these three
     id3_c ::  r -> (( d -> c )  -> r)       -- should be
     id3_r ::  r -> (  d -> c )  -> r        -- the same?


-- | A variant of '<*>' with the arguments reversed.
(<->) :: I f => f a -> f (a -> b) -> f b
(<->) = liftA2 (flip ($))



-- | Lift a function to actions.
-- This function may be used as a value for `fmap` in a `Functor` instance.
liftA  :: I f => (a -> b)           -> f a -> f b
liftA2 :: I f => (a -> b -> c)      -> f a -> f b -> f c
liftA3 :: I f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d

liftA  f a     = (|\/|) f <*> a
liftA2 f a b   = f <$> a <*> b
liftA3 f a b c = f <$> a <*> b <*> c



-- Instances for:
--
--   (Identity a)
-- 
--
--   (->)
--   ([])
--   (Maybe)
--
--   ((,) a)        <=   Monoid a
--   (Constant a)   <=   Monoid a
----------------------------------------------------------------------------------------------------------------------------------------
instance I ((->) environment) where
     (|\/|)   env   = (\x -> env        )     -- pure = const
     (|--|>|) l r   = (\x -> (l x) (r x))     -- <*> f g x = f x (g x)

     i  = pure id
     b  = pure (.)
     c  = pure (flip)
     s  = pure (ap)

instance I [] where
     (|\/|)    = return
     (|--|>|)  = ap

     i = pure id
     b = pure (.)
     c = pure (flip)
     s = pure (ap)

instance I Maybe where
     (|\/|)    = return
     (|--|>|)  = ap

     i = pure id
     b = pure (.)
     c = pure (flip)
     s = pure (ap)


instance I Identity where
    (|\/|)   env                         = Identity  env
    (|--|>|) (Identity l) (Identity r)   = Identity (l r)
    i = pure id
    b = pure (.)
    c = pure (flip)
    s = pure (ap)

instance Monoid a => I ((,) a) where
     (|\/|) v                    = (mempty       , v         )
     (|--|>|) (l,left) (r,right) = (l `mappend` r, left right)
     i = pure id
     b = pure (.)
     c = pure (flip)
     s = pure (ap)


instance Monoid a => I (Constant a) where
    (|\/|)   _________________________  = Constant mempty
    (|--|>|) (Constant x) (Constant y)  = Constant (x `mappend` y) 
    i = pure id
    b = pure (.)
    c = pure (flip)
    s = pure (ap)





--------------------------------------------------------------------------------------------------------------
-- The following two are identical!

instance I IO where
     (|\/|)    = return
     (|--|>|)  = ap
     i = pure id
     b = pure (.)
     c = pure (flip)
     s = pure (ap)

{-
instance A IO where
     (|\/|)   env   = return env
     (|--|>|) l r   = do { f <- l; c <- r; return (f c) }
-}
--
--------------------------------------------------------------------------------------------------------------
