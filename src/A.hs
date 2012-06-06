module Control.A where

import Prelude hiding ((||),(&&),(<=),(>=),(==))

import Data.Functor.Identity (Identity(..))
import Data.Functor.Constant (Constant(..))
import Control.Monad (ap)
import Data.Monoid (Monoid(..))

infixl 6 |--|>| -- <>    in paper  , <*>  in Haskell         -- hinze uses 6, control.applicative uses 4
infixl 6 |\/|   -- pure  in paper  , pure in Haskell
infixl 6 |--|   -- (*)   in paper  ,      in Haskell         -- that is, this isn't defined in Haskell

-- |
----------------------------------------------------------------------------------------------------------------------------------------
-- A           {- Symmetric Interface -}
----------------------------------------------------------------------------------------------------------------------------------------
class A the where                                   -- has the same kind as Functor, namely (* -> *)      -> Constraint
   (|\/|)                :: v -> the v
   (|--|>|)              :: the (l -> r) -> (the l -> the r)

   -- A has four Laws:
   -- (0) A   Identity   :   pure iId         <*> u           = u
   -- (1) A   Composition:   pure (.)         <*> u <*> v <*> w = u <*> (<*> v w)
   -- (2) The Homomprhism:   pure f <*> pure x                = pure (f x)
   -- (3) The Interchange:                       u <*> pure x = pure (<*> x) <*> u

     law0_0 u = ( (|\/|) id )
                   |--|>| u

     law0_1 u = liftA (id) u

-- |
----------------------------------------------------------------------------------------------------------------------------------------
-- A           {- Symmetric Interface (with flip) -}
----------------------------------------------------------------------------------------------------------------------------------------
class LinearA the where                                   -- has the same kind as Functor, namely (* -> *)      -> Constraint
   (|---|)                :: v -> the v
   (|---|>|)              :: the (l -> r) -> (the l -> the r)
   (|---|<|)              :: the (l -> c -> r) -> the c -> the (l -> r)

----------------------------------------------------------------------------------------------------------------------------------------
-- S           {- "Asymmetric" Interface -}
----------------------------------------------------------------------------------------------------------------------------------------

class S the where                                                               -- has kind (* -> *) -> * -> Constraint
     unit                :: the ()                                              -- normally ()
     vmap                :: (l -> r) -> ((the l)  -> (the r))
     (|--|)              ::              (the l)  -> (the r) -> the (l,r)
     unit                = pure ()
     vmap l r            = vmap app ((pure l) |--| r)
     
     -- This is *not* part of this typeclass, but otherwise Haskell can't infer (A ..) context!
     pure                :: r -> the r
     pure                = (\f -> vmap (const f) unit)

     -- S has six laws where @u = unit@:
     --
     -- (0) F-identity   :    map id x            = x
     -- (1) F-composition:    map (l . r) x       = map l (map r x)
     -- (2) *-naturality :    map (l x r) (x * y) = map l x * map r y
     -- (3) snd |-!| id  :    map   snd   (u * y) =                 y
     -- (4) fst |!-| id  :    map   fst   (x * u) =       x
     -- (5) associativity:


app (f,x) = f x

----------------------------------------------------------------------------------------------------------------------------------------
-- A           {- Symmetric Interface -}
--
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

-- with Monoid too
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


liftA :: A a => (l -> r) -> a l -> a r
liftA l r =   (|\/|)  l 
               |--|>| r



-- A laws:




-- ^^^^^^^ think about a pixel-fluid floating downward on the screen. 
--   The topology of the |\/| is like a pachinco (sp?) board - it would sieve the
--   the water (or particles) so that *many* more particles will make it through the open slits.
--    that is, through the left, center, and right "channels"

