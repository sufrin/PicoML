/*
        Playing with type classes (not type constructor classes)
        without using records. This is what convinced me that we
        really needed records in the language.
*/

data Monoid a = Monoid (  a,        --- unit
                          (a,a)->a  --- (#)
                       );                     
                       
let instance_Monoid_Num  = Monoid (0, (+));
    instance_Monoid_Bool = Monoid (True, (||));;

                         
let fold: Monoid a -> [a] -> a;
    fold (Monoid(unit, (#))) = let rec f = \( [] -> unit | (x::xs) -> x # f xs ) in f;;

let ∑  = fold (Monoid(0, (+)));
    ∏  = fold (Monoid(1, (*)));
    all= fold (Monoid(True, (&&)));
    some = fold (Monoid(True, (||)));;
    
--- Equivalence types    

data Eq a = Eq ( (a,a)->Bool ); --- (==)

let structural_Eq = Eq((=));;
   
data Set a =  Empty | Single a | (Set a) `Union` (Set a);

let foldSet empty single (`union`) = 
    let rec fold = 
    \
    ( Empty       -> empty
    | Single a    -> single a
    | l `Union` r -> fold l `union` fold r
    ) in fold;;

let mem: Eq a -> (a, Set a) -> Bool;
    mem (Eq (==)) (x, s) = foldSet False (==x) (||) s;;

let subset: Eq a -> (Set a, Set a) -> Bool;
    subset eq (s1, s2) = 
    let (<-) = mem eq
    in  foldSet True (<- s2) (&&) s1;;

let instance_Eq_Set : @a.Eq a->Eq(Set a);
    instance_Eq_Set instance_Eq = 
    let (<=)   = subset instance_Eq in
    let s1==s2 = (s1 <= s2) && (s2 <= s1)
    in Eq(==);;

--- tests
let s12 = Single 1 `Union` Single 2;;
let s21 = Single 2 `Union` Empty `Union` Single 1;;

let main = let Eq(==) = instance_Eq_Set structural_Eq in s12==s21;;






