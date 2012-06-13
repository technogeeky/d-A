module Control.Applicative.S where
----------------------------------------------------------------------------------------------------------------------------------------
-- S           {- "Ssymmetric" Interface -}
----------------------------------------------------------------------------------------------------------------------------------------

class (Functor a) => S a where                                             -- has kind (* -> *) -> * -> Constraint
     unit                :: a ()                                           -- normally ()
     smap                :: (l -> r) -> ((a l)  -> (a r))
     (|--|)              ::              (a l)  -> (a r) -> a (l,r)

     unit                = spure ()
     smap l r            = smap app ((spure l) |--| r)
     
     -- This is *not* part of this typeclass, but otherwise Haskell can't infer (A ..) context!
     spure                :: r -> a r
     spure                = (\f -> smap (const f) unit)

     -- S has six laws where @u = unit@:
     --
     -- (0) F-identity   :    map id x                 = x
     -- (1) F-composition:    map (l . r) x            = map l (map r x)
     -- (2) *-naturality :    map (l x r) (x * y)      = map l x * map r y
     -- (3) snd |-!| id  :    map   snd   (u * y)      =                 y
     -- (4) fst |!-| id  :    map   fst   (x * u)      =       x
     -- (5) associativity:    map assocl (a * (b * c)) = (a * b) * c

app (f,x) = f x


