module Control.Dimension where

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


