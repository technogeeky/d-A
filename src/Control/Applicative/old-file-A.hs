module Control.Applicative.A where

import Prelude hiding ((||),(&&),(<=),(>=),(==))

import Data.Functor.Identity (Identity(..))
import Data.Functor.Constant (Constant(..))
import Control.Monad (ap)
import Data.Monoid (Monoid(..))

import Control.Dimension

infixl 6 |--|>| -- <>    in paper  , <*>  in Haskell         -- hinze uses 6, control.applicative uses 4
infixl 6 |\/|   -- pure  in paper  , pure in Haskell
infixl 6 |--|   -- (*)   in paper  ,      in Haskell         -- that is, this isn't defined in Haskell

-- ----------------------------------------------------------------------------------------------------------------------------------------
-- A           {- Symmetric Interface (with flip) -}
----------------------------------------------------------------------------------------------------------------------------------------
class LinearA idi where                                   -- has idi same kind as Functor, namely (* -> *)      -> Constraint
   (|---|)                :: v -> idi v
   (|---|>|)              :: idi (l -> r) -> (idi l -> idi r)
   (|---|<|)              :: idi ((l -> c -> r) -> idi c -> idi (l -> r))

class A idi => WeakA idi where
     i = (|\/|) id



liftA :: A a => (l -> r) -> a l -> a r
liftA l r =   (|\/|)  l 
               |--|>| r



-- A laws:




-- ^^^^^^^ think about a pixel-fluid floating downward on the screen. 
--   The topology of the |\/| is like a pachinco (sp?) board - it would sieve the
--   the water (or particles) so that *many* more particles will make it through the open slits.
--    that is, through the left, center, and right "channels"

