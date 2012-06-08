d-A
===

A is for Applicative

#Typing Rules

### page 1

If Cats and Dogs are catagories, then we write: f :: Cats -> Dogs to express that F is a functor between Cats and Dogs.

    A :: Cats         f :: A -> (B :: Cats)
    -----------       ------------------
    F A :: Dogs       F f :: F A -> (F B :: Dogs)

    "The arrow part respects the types."

    + The id and composition laws.
    
The F in "F f"           is  the arrow  part.
The F in "F A" and "F B" are the object parts.


### page3

What it actually says:

    A :: Cats         f   :: B -> (A :: Cats)
    -----------       ------------------
    A :: Cats^O       f^O :: A -> (B :: Cats^O)

What I am hoping it should say:

    A :: Cats         f     :: B -> (A :: Cats)
    -----------       ------------------
    F A :: Cats^O     F f^O :: F A -> (F B :: Cats^O)


### page3 bifunctor


    OutL (L,R) = L
    OutR (L,R) = R
    OutL (f,g) = f
    OutR (f,g) = g


Typing Rules:

         P :: (Cats x Dogs)           p ::      P ->      Q  :: (Cats x Dogs)
    ----------------------      -----------------------------
    OutL P ::  Cats              OutL p :: OutL P -> OutL Q  :: Cats


         P :: (Cats x Dogs)           p ::      P ->      Q  :: (Cats x Dogs)
    ----------------------      -----------------------------
    OutL P ::         Dogs       OutR p :: OutR P -> OutR Q  ::         Dogs



### page4

    f :: M -> Y :: Cats
    g :: W -> Z :: Dogs
    
    
    M :: Cats   W :: Dogs     f :: M -> W :: Cats   g :: Y -> Z :: Dogs
    ----------------------    -----------------------------------------
    (M,W) :: (Cats x Dogs)    (f,g) :: (M,Y) -> (W,Z) :: (Cats x Dogs)
    



### page5

    F :: Dogs^Cats      a :: F -> G :: Dogs^Cats
    ---------------     -------------------------
    F A :: Dogs         a A :: F A -> G A :: Dogs



    F :: Cats -> Dogs     a :: F -> G
    ------------------    -----------
    F :: Dogs^Cats        a :: F -> G :: Dogs^Cats
    



    F :: Dogs^Cats   A :: Cats    a :: F -> G :: Dogs^Cats   f :: A -> B :: Cats
    --------------------------    -----------------------------------------------
         F * A :: Dogs            a * f :: F * A -> G * B  :: Dogs


### page6

    A :: Cats                 f :: A -> A' :: Cats
    -------------------       -----------------------------------------------------
    /\ F A :: Emus^Dogs       /\ F f  ::  /\ F A   ->  /\ F A' :: Emus^Dogs


    B :: Dogs                 g :: B -> B' :: Dogs
    -------------------       -----------------------------------------------------
    /\ F A B :: Emus          /\ F A g  ::  /\ F A B   ->  /\ F A B' :: Emus
    


    x :: C |-- x :: C 
    
    
    g :: D -> E     |
       and          |===    \ x . g (f x)  :: C -> E
    f :: C -> D     |


    Gamma , x :: t |-- e :: u
    -------------------------
    Gamma |-  \ x . e :: t -> u
    
    
suggests that we must consider:

    g :: D -> E     |
       and          |
    f :: C -> D     |=== g (f x)  :: E
       and          |
    x :: C          |
    


### page7


     G :: Emus^Dogs   F :: Dogs^Cats      b :: G -> K :: Emus^Dogs    a :: F -> H :: Dogs^Cats
     -------------------------------      -------------------------------------------
        G . F :: Emus^Cats                b . a ::  G . F -> K . H  :: Emus^Cats


    a :: C  |
     and    |==  a x b :: C
    b :: C  |
    
    a :: C  |
     and    |==  a + b :: C
    b :: C  |

