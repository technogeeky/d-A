{-# LANGUAGE GADTs #-}

module Control.Applicative.I.GADT where

-- @
-- from: 
-- http://www.cse.unsw.edu.au/~chak/haskell/term-conv/DeBruijn.hs
-- @


data Ix e a where
    OOZ  :: {- Void  ()  () -> -} Ix (e, a) a      -- awo O(rigin)s         give  you a Z(ero)
    OZI  ::    Ix    e   a  ->    Ix (e, s) a      -- One O(rigin), one Z   gives you an Identity

minus1 = OOZ             -- not a valid         typing context -- Ix (e,0)       0 -- relative to what e?
zero   = OZI OOZ         -- the first possible  typing context -- Ix ((e,1),0)   0 -- ok, we pick the 0
one    = OZI (OZI OOZ)   -- the second possible typing context -- Ix ((e,1),0)   1 -- ok, "  "     "  1


instance Show (Ix env t) where
  show = show . ixToInt
    where
      ixToInt :: Ix env t     -> Int
      ixToInt OOZ              = 0
--      ixToInt (OZI n)          = (ixToInt n) + 1

data Term env t where
  Con :: {-(Show t) => -}                 t    
                                               -> Term  env       t
  Lam ::
                                                  Term (env, s)   t
                                               -> Term  env (s -> t)
  App ::
                                                  Term  env (s -> t) 
                                               -> Term  env  s 
                                               -> Term  env       t
-------------------------------------------------------------------------
  Var ::                  Ix    env       t                                -- both interfaces share Var!
                                               -> Term  env       t
-------------------------------------------------------------------------
  Pair ::
                                                  Term  env   s 
                                               -> Term  env        t 
                                               -> Term  env  (s  , t)
  Map ::
                               (s -> t) ->      ( Term  env   s 
                                                ->Term  env        t
                                                )
  Unit ::                                         
                                                  Term env ()
  
data       Env i v  where
  O  :: Env i ()                             -- "Empty"
  I  :: Env i env -> i a -> Env i (env,a)    -- "Push"




instance Show (Term env t) where
  show (Var ix)      = "{" ++ show ix ++ "}"
--  show (Con c)       = show c
  show (Lam body)    = "(\\" ++ show body ++ ")"
  show (App fun arg) = "(" ++ show fun ++ " " ++ show arg ++ ")"






acc :: Ix e a -> Env i e -> i a

acc (OOZ)   (I _ u) = u
acc (OZI x) (I n u) = acc x n


runIDI :: Term e x -> Env i e -> i x

runIDI c@(Con u)   n = undefined             -- should be: pure u
runIDI v@(Var x)   n = acc x n     -- good!
runIDI a@(App x y) n = undefined             -- should be: ((runIDI x) n) <> ((runIDI y) n)


ex1 :: Term (( (), Integer), Integer) Integer
ex1 = App (App (Con (+)) (Var zero)) (Var minus1)

eta :: (((), Integer -> Integer), Integer -> Integer)
eta = (( (), \n -> 2*n ), \n -> 2*n + 1 )


eta_env :: Env ((->) Integer) (( (), Integer), Integer)
eta_env = undefined  



-- ^
-- >>> :k Env ((->) Integer)
-- Env ((->) Integer) :: * -> *
--
-- ^
-- >>> :k Env ((->) Integer) (((), Integer), Integer)
-- Env ((->) Integer) (((), Integer), Integer) :: *


blah = runIDI ex1 eta_env

norm :: Term env a -> Term env a

norm e = case norm1 of Map f u -> case norm2 of Map g v ->  Map (f . g) v
          where norm1 = undefined
                norm2 = undefined

norm1 :: Term env a -> Term env a
norm1 (Var n)      =                                 Map  (id)  (Var n)
norm1 (Unit)       =                                 Map   id    Unit
norm1 (Map f e)    = case norm1 e of
      (Map g u)                                   -> Map (f . g)   u
norm1 (Pair e1 e2) = case (norm1 e1 , norm1 e2 ) of 
                          (Map f1 u1, Map f2 u2)  -> Pair (Map f1 u1) (Map f2 u2)


norm2 :: Term env a -> Term env a
norm2 (Var n)      =                                 Map  (id)  (Var n)
norm2 (Unit)       =                                 Map   id    Unit

norm2 (Pair Unit e2)     = case ( {- -----,-} norm2 e2  ) of 
                                ( {- -----,-} Map f2 u2 )   -> Pair (Unit) (Map f2 u2)

norm2 (Pair e1 Unit)     = case ( norm2 e1  {-, ----- -} ) of 
                                ( Map f1 u1 {-, ----- -} )  -> Pair (Map f1 u1) (Unit)

norm2 (Pair e1 (Var n))  = case ( norm2 e1  {-, ----- -} ) of 
                                ( Map f1 u1 {-, ----- -} )  -> Pair (Map f1 u1) (Map id (Var n))

-- Why was this "case" left out? Because we know the Var will always be in the "snd"?
norm2 (Pair (Var n) e2)  = case ( {- -----,-} norm2 e2  ) of 
                                ( {- -----,-} Map f2 u2 )   -> Pair (Map id (Var n)) (Map f2 u2)


norm2 (Pair e1 (Pair e2 e3)) = case 
                                (norm2 (Pair (Pair e1 e2) e3)) of
                                (Map f u                     )   -> Map (assocr . f) u


-- taken from hackage/pointless-haskell

infix 6  /\
-- | The infix split combinator.
(/\) :: (a -> b) -> (a -> c) -> a -> (b,c)
(/\) f g x = (f x, g x)

infix 7  ><
-- The infix product combinator.
(><) :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
f >< g = f . fst /\ g . snd

-- | Associates nested products to the right.
assocr :: ((a,b),c) -> (a,(b,c))
assocr = fst . fst  /\  (snd >< id)

infix 4 \/
-- | The infix either combinator.
(\/) :: (b -> a) -> (c -> a) -> Either b c -> a
(\/) = either



-- More roman-like numerals:
--  OII  :: 
--  III  :: 
--  IVO
--  OVO
--  OVI
--  VII
--  IIX
--  IXO
--  OXO
--  OXI
--  XII
--  III -- already exists. stop!
