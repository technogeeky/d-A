{-# LANGUAGE UnicodeSyntax #-}
module ICalculus where


----------------------------------------------------------------------------------------------------------------------------------------
-- Syntax
----------------------------------------------------------------------------------------------------------------------------------------

data Ix :: (* -> * -> *) where
Zero :: Ix (rho,alpha ) alpha
Succ :: Ix rho beta -> Ix (rho,alpha ) beta

data Term :: * -> * -> * where
Con :: alpha -> Term rho alpha
Var  :: Ix rho alpha -> Term rho alpha
App :: Term rho (alpha -> beta) -> Term rho alpha -> Term rho beta.

ex1  :: Term ((rho,Integer),Integer) Integer
ex1 = App (App (Con (+)) (Var (Succ Zero))) (Var Zero).


-- Symmetric Interface
Map :: (alpha -> beta) -> (Term rho alpha -> Term rho beta)
Unit :: Term rho ()
Pair :: Term rho alpha -> Term rho beta -> Term rho (alpha,beta ).


-- We sometimes use numeric literals for De Bruijn indices and abbreviate App u v by u :<> v.

-- Pair, infix, is :*



----------------------------------------------------------------------------------------------------------------------------------------
-- Semantics
----------------------------------------------------------------------------------------------------------------------------------------

data Env :: (* -> *) -> * -> * where
Empty :: Env ι ()
Push   :: Env ι rho -> ι alpha -> Env ι (rho,alpha ).

η1  :: Env (Integer -> ) (((),Integer),Integer)
η1 = hhhi,λn -> 2 * ni,λn -> 2 * n + 1i.

acc :: Ix rho alpha -> Env ι rho -> ι alpha
acc Zero       hη,ui = u
acc (Succ n) hη,ui = acc n η


Lemma 1  (Normalform) Let e be an idiomatic term that contains the list of variables
Var i1, . . . , Var in. Then e is equivalent to
1.  Con f : Var i1 : ··· : Var in for some suitable f , and to
2.  Map g : (Var i1 :? ··· :? Var in ) for some suitable g.

Proof  Part 1 is the curried version of Part 2, so it sufﬁces to prove the latter. Since we have
formalised the syntax and semantics of idiomatic expressions in Haskell, we can actually
program the normalisation. Assuming the symmetric interface, we proceed in two steps.
First, we move all occurrences of Map to the front: e is transformed into Map f u where u
is a nested pair of variables and units. Second, we turn u into a left-linear tree, that is, a
‘snoc-list’ of variables.

norm0    :: Term rho alpha -> Term rho alpha
norm0 e = case norm1 e of Map f u -> case norm2 u of Map g v -> Map (f · g) v

norm1 :: Term rho alpha -> Term rho alpha
norm1 (Var n)    = Map id (Var n)   -- functor identity
norm1 (Map f e) = -- functor composition
case norm1 e of Map g u -> Map (f · g) u
norm1 Unit         = Map id Unit        -- functor identity
norm1 (e1 :? e2)  = -- naturality of ?
case (norm1 e1,norm1 e2) of (Map f1 u1,Map f2 u2) -> Map (f1 × f2) (u1 :? u2)

norm2 :: Term rho alpha -> Term rho alpha
norm2 (Var n) = Map id (Var n) -- functor identity
norm2 Unit = Map id Unit -- functor identity
norm2 (Unit :? e2)       =
case norm2 e2 of Map f2 u2 -> Map (const () M f2) u2     -- left identity
norm2 (e1 :? Unit)       =
case norm2 e1 of Map f1 u1 -> Map (f1
M const ()) u1     -- right identity
norm2 (e1 :? Var n)      =
case norm2 e1 of Map f1 u1 -> Map (f1 × id) (u1 :? Var n)
norm2 (e1 :? (e2 :? e3)) = -- associativity
case norm2 ((e1 :? e2) :? e3) of Map f u -> Map (assocr · f ) u


--  id M const () is the inverse of fst :: (alpha,()) -> alpha.



----------------------------------------------------------------------------------------------------------------------------------------
-- * The Y-I-K Calculus
----------------------------------------------------------------------------------------------------------------------------------------

Abs :: Term (rho,alpha ) beta -> Term rho (alpha -> beta)

----------------------------------------------------------------------------------------------------------------------------------------
-- ** Definition 1 
----------------------------------------------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------------------------------------------
-- ** Definition 2
----------------------------------------------------------------------------------------------------------------------------------------

Deﬁnition 2  (Combinatory model condition) 

An idiom ι has K-combinators iff there exist elements 
     k :: ι (alpha -> beta -> alpha)                       -- and 
     s :: ι ((alpha -> beta -> γ) -> (alpha -> beta) -> (alpha -> γ)) -- satisfying

k <*> u <*> v       = u
s <*> u <*> v <*> w = (u <*> w) <*> (v <*> w),



abs :: Term (rho,alpha ) beta -> Term rho (alpha -> beta)
abs (Con u)          = K : Con u
abs (Var Zero)      = I
abs (Var (Succ n)) = K : Var n
abs (e1 : e2)        = S : abs e1 : abs e2.




For strongly extensional idioms with K-combinators, left and right identity laws can be
strengthened to

pure fst  (u ? v)   = u (1)
pure snd  (u ? v) = v. (2)


acc :: Ix rho alpha -> Env Id rho -> alpha
acc Zero       = snd
acc (Succ n) = acc n · fst

dist :: (Idiom ι) ⇒ Env ι rho -> ι (Env Id rho)
dist hi       = unit
dist hvs,vi = dist vs ? v.

----------------------------------------------------------------------------------------------------------------------------------------
-- ** Definition 3
----------------------------------------------------------------------------------------------------------------------------------------

Deﬁnition 3  Let ι and κ be idioms. A natural transformation h :: ι alpha -> κ alpha is an idiom
homomorphism iff h preserves pure computations and idiomatic application, that is,
h (pure a) = pure a (4)
h (u  v)   = h u  h v, (5)
for all a, u, v of the appropriate types.


Lemma 4  Let ι and κ be idioms, and let h :: ι alpha -> κ alpha be an idiom isomorphism.
1.  If ι is (strongly) extensional, then κ is (strongly) extensional.
2.  If ι has K-combinators, then κ has K-combinators.


----------------------------------------------------------------------------------------------------------------------------------------
-- ** Definition 2
----------------------------------------------------------------------------------------------------------------------------------------

Deﬁnition 4  (Weak combinatory model condition) 

An idiom ι has I-combinators iff there exist elements 
     i :: ι (alpha -> alpha)                              -- Identity
     b :: ι ((beta -> γ) -> (alpha -> beta) -> (alpha -> γ))        -- (composition)
     c :: ι ((alpha -> beta -> γ) -> beta -> (alpha -> γ)           -- (flip)
     s :: ι ((alpha -> beta -> γ) -> (alpha -> beta) -> (alpha -> γ))    -- pure S

i <*> u = u (6)
b <*> u <*> v <*> w = u <*> (v <*> w) (7)
c <*> u <*> v <*> w = (u <*> w) <*> v (8)
s <*> u <*> v <*> w = (u <*> w) <*> (v <*> w), (9)


abs
0
:: Term (rho,alpha) beta -> Term rho (alpha -> beta)
abs
0
(Var Zero) = I
abs
0
(e1 :<*> e2)
| ¬free e1 ∧ free e2     = B :<*> dec  e1 :<*> abs
0
e2
| free e1 ∧ ¬free e2 = C :<*> abs
0
e1 :<*> dec  e2
| free e1 ∧ free e2     = S :<*> abs
0
e1 :<*> abs
0
e2.

free :: Term rho alpha -> Bool
free (Con u)          = False
free (Var Zero)      = True
free (Var (Succ n)) = False
free (e1 : e2)        = free e1 ∨ free e2,

dec :: Term (rho,alpha) beta -> Term rho beta
dec (Con u)          = Con u
dec (Var (Succ n)) = Var n
dec (e1 : e2)        = dec e1 : dec e2.

Every idiom possesses two of the four combinators: i = pure I = pure id  and b =
pure B = pure (·). Condition (6) and (7) are idiom identity and idiom composition in dis-
guise. Consequently, to establish the weak combinatory model condition it sufﬁces to show
the existence of c and s. Furthermore, if a strongly extensional idiom has I-combinators,
then c = pure C and s = pure S . For instance, the Maybe idiom has I-combinators. On the
other hand, stateful idioms such as IO and parser idioms don’t possess I-combinators as we
can’t re-order or duplicate stateful computations.

