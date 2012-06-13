{-# LANGUAGE GADTs #-}

module Control.Applicative.I.GADT where

-- @
-- from: 
-- http://www.cse.unsw.edu.au/~chak/haskell/term-conv/DeBruijn.hs
-- @


data Ix env t  where
    OOZ  :: {- Void  ()  () -> -}   Ix (env, t) t      -- Two O(rigin)s         give  you a Z(ero)
    OZI  ::    Ix   env   t ->      Ix (env, s) t      -- One O(rigin), one Z   gives you an Identity

instance Show (Ix env t) where
  show = show . ixToInt
    where
      ixToInt :: Ix env t     -> Int
      ixToInt OOZ              = 0
--      ixToInt (OZI n)          = (ixToInt n) + 1

data Term env t where
  Con :: (Show t) => t -> Term  env       t

  Var ::                  Ix    env       t                
                       -> Term  env       t

  Lam ::                  Term (env, s)   t
                       -> Term env  (s -> t)

  App ::                  Term env  (s -> t) 
                       -> Term env  s 
                       -> Term env        t

  Map :: (s -> t)      -> 
                      (    Term env  s 
                       ->  Term env       t
                      )
  Unit ::                  Term env ()
  
  Pair ::                  Term env  s 
                        -> Term env    t 
                        -> Term env (s,t)

instance Show (Term env t) where
  show (Var ix)      = "{" ++ show ix ++ "}"
  show (Con c)       = show c
  show (Lam body)    = "(\\" ++ show body ++ ")"
  show (App fun arg) = "(" ++ show fun ++ " " ++ show arg ++ ")"




data       Env i v  where
  Empty :: Env i ()
  Push  :: Env i env -> i a -> Env i (env,a)



n = (((), \n -> 2*n), \n -> 2*n + 1)

{-
acc :: Ix e a -> Env i e -> i a
acc (OCZ n) u = u
acc (OZI x) u) = acc x u
-}


{-
norm :: Term env a -> Term env a

norm e = case norm1 of Map f u ->
         case norm2 of Map g v    ->  Map (f . g) v
-}

norm1 :: Term env a -> Term env a
norm1 (Var n)    = Map (id) (Var n)
norm1 (Map f e)  = case norm1 e
                    of  Map g u -> Map (f . g) u
norm1 (Unit)     = Map id Unit
norm1 (App e1 e2) = case (norm1 e1, norm1 e2) of (Map f1 u1, Map f2 u2) -> Map (f1,f2) (u1,u2)


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
