module Control.A where

import Prelude hiding ((||),(&&),(<=),(>=),(==))

import Data.Functor.Identity (Identity(..))
import Data.Functor.Constant (Constant(..))
import Control.Monad (ap)
import Data.Monoid (Monoid(..))

infixl 6 |--|>| -- <>    in paper  , <*>  in Haskell         -- hinze uses 6, control.applicative uses 4
infixl 6 |\/|   -- pure  in paper  , pure in Haskell
infixl 6 |--|   -- (*)   in paper  ,      in Haskell         -- that is, this isn't defined in Haskell


newtype Vi   i = OutI    { out          :: i (Vi i)                             }
newtype Vii  i = OutII   { outII        :: i (Vi i, Vi i)                       }
newtype Viii i = OutIII  { outIII       :: i (Vi i, Vi i, Vi i)                 }
newtype IV   i = OutIV   { outIV        :: i (Vi i, Vi i, Vi i, Vi i)           }
newtype V    i = OutV    { outV         :: i (Vi i, Vi i, Vi i, Vi i, Vi i)     }
newtype Ao   o = InaO    { (|-°|)       :: o (Ao o)                             }
newtype Aoo  o = InaOO   { (|--°|)      :: o (Aoo o, Ao o)                      }
newtype Aooo o = InaOOO  { (|---°|)     :: o (Aooo o, Aoo o, Ao o)              }
newtype OA   o = InaOA   { (|°----|)    :: o (OA o, Aooo o, Aoo o, Ao o)        }
newtype An   o = InaA    { (|°/\°|)     :: o (Ao o, Ao o, Ao o, Ao o, Ao o)     }

curry1   :: ((a) -> b)        -> a -> b
curry2   :: ((a,b) -> c)      -> a -> b -> c
curry3   :: ((a,b,c) -> d)    -> a -> b -> c -> d
curry4   :: ((a,b,c,d) -> e)  -> a -> b -> c -> d -> e
curry5   :: ((a,b,c,d,e) -> f)-> a -> b -> c -> d -> e -> f


uncurry1 :: (a -> b)                     -> (a)     -> b
uncurry2 :: (a -> b -> c)                -> (a,b)   -> c
uncurry3 :: (a -> b -> c -> d)           -> (a,b,c) -> d
uncurry4 :: (a -> b -> c -> d -> e)      -> (a,b,c,d)   -> e
uncurry5 :: (a -> b -> c -> d -> e -> f) -> (a,b,c,d,e) -> f

curry1 f x          = f (x)
curry2 f x y        = f (x,y)
curry3 f x y z      = f (x,y,z)
curry4 f x y z a    = f (x,y,z,a)
curry5 f x y z a b  = f (x,y,z,a,b)

uncurry1 f (x) = f x
uncurry2 f (x,y) = f x y
uncurry3 f (x,y,z) = f x y z
uncurry4 f (x,y,z,a) = f x y z a
uncurry5 f (x,y,z,a,b) = f x y z a b

class A the where                                   -- has the same kind as Functor, namely (* -> *)      -> Constraint
   (|\/|)                :: v -> the v
   (|--|>|)              :: the (l -> r) -> (the l -> the r)

class LinearA the where                                   -- has the same kind as Functor, namely (* -> *)      -> Constraint
   (|---|)                :: v -> the v
   (|---|>|)              :: the (l -> r) -> (the l -> the r)
   (|---|<|)              :: the (l -> c -> r) -> the c -> the (l -> r)
   -- A has four Laws:
   -- (0) A   Identity   :   pure iId         <> u           = u
   -- (1) A   Composition:   pure (.)         <> u <> v <> w = u <> (<> v w)
   -- (2) The Homomprhism:   pure f <> pure x                = pure (f x)
   -- (3) The Interchange:                       u <> pure x = pure (<> x) <> u

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


instance A ((->) environment) where
     (|\/|)   env   = (\x -> env        )     -- pure = const
     (|--|>|) l r   = (\x -> (l x) (r x))     -- <*> f g x = f x (g x)


instance A [] where
     (|\/|)    = return
     (|--|>|)  = ap



instance A Maybe where
     (|\/|)    = return
     (|--|>|)  = ap

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

instance Monoid a => A ((,) a) where
     (|\/|) v                    = (mempty       , v         )
     (|--|>|) (l,left) (r,right) = (l `mappend` r, left right)


instance Monoid a => A (Constant a) where
    (|--|>|) (Constant x) (Constant y)  = Constant (x `mappend` y) 
    (|\/|)   _________________________  = Constant mempty


instance A Identity where
    (|\/|)   env                         = Identity  env
    (|--|>|) (Identity l) (Identity r)   = Identity (l r)




liftA :: A a => (l -> r) -> a l -> a r



-- "should be pure l <*> r"?
--


-- test2.hs:86:13: parse error on input `|\/|'
-- Failed, modules loaded: none.

liftA l r = (|\/|)  l |--|>| r

-- works fine




{-
liftA l r = ( (|\/|)  l    ) 
               |--|>| r
-}
--liftA f a = ( |--|>|  )       -- this is not the order of operations I would normally think in..
--            ((|\/|) f)        -- this is supposed to be pure a <*> f
--                    a 


-- A laws:

idiom_identity u = ( (|\/|) id )
                      |--|>| u

idiom_identity2 u = liftA (id) u

-- if you don't use () in (|\/|) f here, you get this:
{-
test2.hs:93:14:
    Could not deduce (a ~ (->) (l -> r))
    from the context (A a)
      bound by the type signature for
                 liftA :: A a => (l -> r) -> a l -> a r
      at test2.hs:(92,1)-(94,21)
      `a' is a rigid type variable bound by
          the type signature for liftA :: A a => (l -> r) -> a l -> a r
          at test2.hs:92:1
    Expected type: a (l -> r)
      Actual type: (l -> r) -> l -> r
    In the first argument of `(|--|>|)', namely `(|\/| f)'
    In the expression: (|--|>|) (|\/| f) a
    In an equation for `liftA': liftA f a = (|--|>|) (|\/| f) a
Failed, modules loaded: none.
-}


-- ^^^^^^^ think about a pixel-fluid floating downward on the screen. 
--   The topology of the |\/| is like a pachinco (sp?) board - it would sieve the
--   the water (or particles) so that *many* more particles will make it through the open slits.
--    that is, through the left, center, and right "channels"

