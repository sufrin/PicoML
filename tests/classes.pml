---
--- Show a: a has a show function
---

type Show(a) = { show: a->String };;

let print (show: Show(a)) x = putStr(show.show x);;

let showBool: Show(Bool);
    showBool = { show = λ ( True -> "True" | False -> "False" ) }
    ;;
    
let showString: Show(String);
    showString  = { show = λ s -> '"' :: s ++ ['"'] }
    ;;
    
let showNum: Show(Num);
    showNum  = { show = num2string }
    ;;
    
let showList: Show(a) -> Show([a]);
    showList (show: Show(a)) = show ⫻ 
                               let rec t = 
                                 ( 
                                 | []      -> "]"
                                 | [x]     -> show x ++ "]"
                                 | (x::xs) -> show x ++ ", " ++ t xs
                                 )
                               in  
                               { show = \ xs -> "[" ++ t xs };;

---

type Eq a = { (=): (a,a)->Bool };;
  
type Semigroup a = { (⊗): (a,a)->a } with Eq a;;        --- ⊗ associative

type Monoid a = { unit: a } with Semigroup(a);;         --- unit⊗a=a⊗unit=a

--- 
--- NOT Haskell: class (Semigroup a, { unit: a }) => Monoid a where { ... } 
--- 
let theMonoid: (Semigroup a, a) -> Monoid a;            --- embed by /selecting/ the  unit
    theMonoid (s: Semigroup a, unit: a) = 
          s ⫻
          { unit = unit; 
            (=)  = (=);
            (⊗)  = (⊗)
          };;

let theSemigroup: ((a,a) -> a) -> Semigroup a;          --- semigroup with structural equality
    theSemigroup op = { (⊗) = op; (=) = (=) };;

---
--- Haskell: instance Monoid Num where { (⊗)=Pervasives.(*); unit=1 }
--- 
let mulMonoid  = theMonoid ( theSemigroup (*), 1 );;
let addMonoid  = theMonoid ( theSemigroup (+), 0 );;

---




