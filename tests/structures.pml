--- Structures and functors as records and record-functions

type Monoid a = { unit: a;
                  (⊕):  (a,a)->a 
                };;

type Foldoid  a = 
                { lfold: [a]->a; 
                  fold:  [a]->a
                } with Monoid a;;

let folds: Monoid a -> Foldoid a;
    folds (m: Monoid a) = m ⫻
    {   fold  = let f     = ⟨ [] -> unit | (x::xs) -> x ⊕ f xs ⟩ in f;
        lfold = let f acc = ⟨ [] -> acc  | (x::xs) -> f (acc⊕x) xs ⟩ in f unit;
        unit = unit; --- copy from m
        (⊕)=(⊕)      --- copy from m
           
    };;


--- Same as folds but with structure value inheritance avoiding the copying  
let foldt: Monoid a -> Foldoid a;
    foldt (m: Monoid a) = m ⫻
    {   fold  = let f     = ⟨ [] -> unit | (x::xs) -> x ⊕ f xs ⟩ in f;
        lfold = let f acc = ⟨ [] -> acc  | (x::xs) -> f (acc⊕x) xs ⟩ in f unit
    } with m;;

    
type List a = Monoid [a];
let  (list: List a) = { unit=[]; (⊕)=(++) };;
    



