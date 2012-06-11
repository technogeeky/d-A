{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

module D.A where


import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Functor.Identity
import Data.Functor.Constant
import Data.Functor.Compose
import Data.Monoid
import Data.Maybe


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

curry1   :: ((a) -> b)        -> a -> b                     -- or b -> a
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


-- | Suppose we are given mutually recursive data types @A@, @B@, and @C@.
-- Here are some definitions of terms.
-- 
-- [@child@] A maximal subexpression of @A@, @B@, or @C@. 
-- A child does not necessarily have to have the same type as the parent.
-- @A@ might have some children of type @B@ and other children of type @C@ or even @A@.
-- 
-- [@children@] A list of all children.
-- In particular children are ordered from left to right.
-- 
-- [@descendant@] Any subexpression of of @A@, @B@, or @C@.
-- Specifically a descendant of an expression is either the expression itself or a descendant of one of its children.
-- 
-- [@family@] A list of all descendant.
-- The order is a context dependent.
-- 'preorderFold' uses preorder, while 'postorderFold' and 'mapFamilyM' uses postorder.
-- 
-- [@plate@] A plate is a record parametrized by a functor @f@ with one field of type
-- @A -> f A@ for each type belonging to the mutually recursive set of types.  For example,
-- a plate for @A@, @B@, and @C@ would look like
-- 
-- @
-- data ABCPlate f = ABCPlate
--                 { fieldA :: A -> f A
--                 , fieldB :: B -> f B
--                 , fieldC :: C -> f C
--                 }
-- @
-- 
-- Although this above is the original motivation behind multiplate,but you can make
-- any structure you want into a 'Multiplate' as long as you satisfy the two multiplate laws listed
-- below.
-- 
-- The names of the functions in this module are based on Sebastian Fischer's Refactoring Uniplate: 
-- <http://www-ps.informatik.uni-kiel.de/~sebf/projects/traversal.html>

-- | A plate over @f@ consists of several fields of type @A -> f A@ for various @A@s.
-- 'Projector' is the type of the projection functions of plates. 
type Projector p a = forall f. p f -> a -> f a

-- | A 'Multiplate' is a constructor of kind @(* -> *) -> *@ operating on 'Applicative' functors
-- having functions 'multiplate' and 'mkPlate' that satisfy the following two laws:
-- 
-- (1) @
-- 'multiplate' 'purePlate' = 'purePlate'
--   where
--     'purePlate' = 'mkPlate' (\\_ -> 'pure')
-- @
-- 
-- (2) @
-- 'multiplate' ('composePlate' p1 p2) = 'composePlate' ('multiplate' p1) ('multiplate' p2)
--   where
--     'composePlate' p1 p2 = 'mkPlate' (\\proj a -> ('Compose' (proj p1 ``fmap`` proj p2 a)))
-- @
--
-- Note: By parametricity, it suffices for (1) to prove 
-- 
-- @
-- 'multiplate' ('mkPlate' (\\_ -> 'Identity')) = ('mkPlate' (\\_ -> 'Identity'))
-- @
-- 
class Multiplate p where
  multiplate :: (Applicative f) => p f -> p f
  mkPlate :: (forall a. Projector p a -> (a -> f a)) -> p f

applyNaturalTransform :: forall p f g. (Multiplate p) => (forall a. f a -> g a) -> p f -> p g
applyNaturalTransform eta p = mkPlate build
 where
  build :: Projector p a -> a -> g a
  build proj = (eta . proj p)

purePlate :: (Multiplate p, Applicative f) => p f
purePlate = mkPlate (\_ -> pure)

emptyPlate :: (Multiplate p, Alternative f) => p f
emptyPlate = mkPlate (\_ _ -> empty)

kleisliComposePlate :: forall p m. (Multiplate p, Monad m) => p m -> p m -> p m
kleisliComposePlate f1 f2 = mkPlate build
 where
  build :: Projector p a -> a -> m a
  build proj = (proj f1 <=< proj f2)

composePlate :: forall p f g. (Multiplate p, Functor g) => p f -> p g -> p (Compose g f)
composePlate f1 f2 = mkPlate build
 where
  build :: Projector p a -> a -> Compose g f a
  build proj a = (Compose (proj f1 `fmap` proj f2 a))

composePlateRightId :: forall p f. (Multiplate p) => p f -> p Identity -> p f
composePlateRightId f1 f2 = mkPlate build
 where
  build :: Projector p a -> a -> f a
  build proj = (proj f1 . traverseFor proj f2)

composePlateLeftId :: forall p f. (Multiplate p, Functor f) => p Identity -> p f -> p f
composePlateLeftId f1 f2 = mkPlate build
 where
  build :: Projector p a -> a -> f a
  build proj a = (traverseFor proj f1 `fmap` proj f2 a)

appendPlate :: forall p o. (Multiplate p, Monoid o) => p (Constant o) -> p (Constant o) -> p (Constant o)
appendPlate f1 f2 = mkPlate build
 where
  build :: Projector p a -> a -> Constant o a
  -- both <* and *> are the same for the Constant applicative functor
  build proj a = (proj f1 a <* proj f2 a)

alwaysM :: forall p f. (Multiplate p, Functor f) => p (MaybeT f) -> p f
alwaysM f = mkPlate build
 where
  build :: Projector p a -> a -> f a
  build proj a = (fromMaybe a) `fmap` (runMaybeT (proj f a))

mChildren :: forall p o. (Multiplate p, Monoid o) => p (Constant o) -> p (Constant o)
mChildren = multiplate

mapChildrenM :: (Multiplate p, Applicative m, Monad m) => p m -> p m
mapChildrenM = multiplate

mapChildren :: (Multiplate p) => p Identity -> p Identity
mapChildren = multiplate

preorderFold  :: forall p o. (Multiplate p, Monoid o) => p (Constant o) -> p (Constant o)
postorderFold :: forall p o. (Multiplate p, Monoid o) => p (Constant o) -> p (Constant o)

preorderFold f  = f                            `appendPlate` multiplate (preorderFold f)
postorderFold f = multiplate (postorderFold f) `appendPlate` f

mapFamily :: (Multiplate p) => p Identity -> p Identity
mapFamily = mapFamilyM

mapFamilyM :: (Multiplate p, Applicative m, Monad m) => p m -> p m
mapFamilyM f = f `kleisliComposePlate` (multiplate (mapFamilyM f))

evalFamily :: (Multiplate p) => p Maybe -> p Identity
evalFamily f = evalFamilyM (applyNaturalTransform (MaybeT . Identity) f)

always :: (Multiplate p) => p Maybe -> p Identity
always f = alwaysM (applyNaturalTransform (MaybeT . Identity) f)

evalFamilyM :: forall p m. (Multiplate p, Applicative m, Monad m) => p (MaybeT m) -> p m
evalFamilyM f = go
 where
  go = mapFamilyM (mkPlate eval)
  eval :: Projector p a -> a -> m a
  eval proj a = maybe (return a) (proj go) =<< (runMaybeT (proj f a))


traverseFor :: (Multiplate p) => Projector p a -> p Identity -> a -> a
traverseFor proj f = runIdentity . proj f

traverseMFor :: (Multiplate p, Monad m) => Projector p a -> p m -> a -> m a
traverseMFor proj f = proj f


foldFor   :: (Multiplate p) =>             Projector p a -> p (Constant o) -> a -> o
unwrapFor :: (Multiplate p) => (o -> b) -> Projector p a -> p (Constant o) -> a -> b

foldFor proj f             = getConstant .         proj f
unwrapFor unwrapper proj f = unwrapper   . foldFor proj f

sumFor      :: (Multiplate p) => Projector p a -> p (Constant (Sum n))      -> a -> n
productFor  :: (Multiplate p) => Projector p a -> p (Constant (Product n))  -> a -> n
allFor      :: (Multiplate p) => Projector p a -> p (Constant All)          -> a -> Bool
anyFor      :: (Multiplate p) => Projector p a -> p (Constant Any)          -> a -> Bool
firstFor    :: (Multiplate p) => Projector p a -> p (Constant (First b))    -> a -> Maybe b
lastFor     :: (Multiplate p) => Projector p a -> p (Constant (Last b))     -> a -> Maybe b

sumFor      = unwrapFor getSum      -- from the newtype
productFor  = unwrapFor getProduct  -- from the newtype
allFor      = unwrapFor getAll      -- from the newtype
anyFor      = unwrapFor getAny      -- from the newtype
lastFor     = unwrapFor getLast     -- from the newtype
firstFor    = unwrapFor getFirst    -- from the newtype
