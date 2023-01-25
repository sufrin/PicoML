---
--- Record types and opening records within expressions
--- Example: if «m: Monoid a» then  «m ⫻ e» ≡ «let unit=m.unit and (⊕)=m.(⊕) in e» 
---          (alternative notation: «m///e»)
---

type Monoid a = { unit: a; (⊕): (a,a)->a };

let fold: Monoid a -> [a] -> a;
    fold (m: Monoid a) = 
         m ⫻ let f = ⟨ [] -> unit | x::xs -> x ⊕ f xs ⟩ in f;;

let lfold: Monoid a -> [a] -> a;
    lfold (m: Monoid a) = 
          m ⫻ let f acc = ⟨ [] -> acc | x::xs -> f (acc⊕x) xs ⟩ in f unit;;

--- 
--- Forcing the syntactic roles of notations to be other than their default roles
--- 

notation id /\ \/ ++/ and left 9 ⨾ ;;


let ∑    = lfold { unit=0;     (⊕)=(+)};
    ∏    = lfold { unit=1;     (⊕)=(*)};
    ++/  = fold  { unit=[];    (⊕)=(++)};
    /\   = fold  { unit=True;  (⊕)=(&&)};
    \/   = fold  { unit=False; (⊕)=(||)};
    (f ⨾ g) x = g(f x);
    ∀ p  = map p ⨾ /\;
    ∃ p  = map p ⨾ \/;;








