module Control.Applicative.A where

import Data.Functor.Identity (Identity(..))
import Data.Functor.Constant (Constant(..))
import Data.Monoid           (Monoid(..)  )
import Control.Monad (ap)


infixl 6 |--|>| -- <>    in paper  , <*>  in Haskell         -- hinze uses 6, control.applicative uses 4
infixl 6 |\/|   -- pure  in paper  , pure in Haskell

infixl 4 <*>, <->

-- |
----------------------------------------------------------------------------------------------------------------------------------------
-- A           {- Asymetric Interface -}
----------------------------------------------------------------------------------------------------------------------------------------
class Functor a => A a where            -- has the same kind as Functor, namely (* -> *)      -> Constraint
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

-- | A variant of '<*>' with the arguments reversed.
(<->) :: A f => f a -> f (a -> b) -> f b
(<->) = liftA2 (flip ($))



-- | Lift a function to actions.
-- This function may be used as a value for `fmap` in a `Functor` instance.
liftA  :: A f => (a -> b)           -> f a -> f b
liftA2 :: A f => (a -> b -> c)      -> f a -> f b -> f c
liftA3 :: A f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d

liftA f a = (|\/|) f <*> a

-- | Lift a binary function to actions.
liftA2 f a b = f <$> a <*> b

-- | Lift a ternary function to actions.
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
instance A ((->) environment) where
     (|\/|)   env   = (\x -> env        )     -- pure = const
     (|--|>|) l r   = (\x -> (l x) (r x))     -- <*> f g x = f x (g x)


instance A [] where
     (|\/|)    = return
     (|--|>|)  = ap

instance A Maybe where
     (|\/|)    = return
     (|--|>|)  = ap


instance A Identity where
    (|\/|)   env                         = Identity  env
    (|--|>|) (Identity l) (Identity r)   = Identity (l r)

instance Monoid a => A ((,) a) where
     (|\/|) v                    = (mempty       , v         )
     (|--|>|) (l,left) (r,right) = (l `mappend` r, left right)


instance Monoid a => A (Constant a) where
    (|\/|)   _________________________  = Constant mempty
    (|--|>|) (Constant x) (Constant y)  = Constant (x `mappend` y) 





--------------------------------------------------------------------------------------------------------------
-- The following two are identical!

instance A IO where
     (|\/|)    = return
     (|--|>|)  = ap

{-
instance A IO where
     (|\/|)   env   = return env
     (|--|>|) l r   = do { f <- l; c <- r; return (f c) }
-}
--
--------------------------------------------------------------------------------------------------------------

