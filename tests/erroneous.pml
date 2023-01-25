--- 
--- Checking consistency of declared vs inferred
---
let ones = 1::ones;
    map f = \ ( []-> [] | x::xs -> f x :: map f xs );;

--- This should yield a "too general" error
let          
    onesError: @a.[a];
    onesError = ones;;

--- Inconsistent    
let
    mapError:  @r,q.(Num->r)->[q]->[r];
    mapError = map;;

let
    mapNonError: @r.(Num->r)->[Num]->[r];
    mapNonError = map;;



--- This should yield a "conflict" error
let   
    mapError3: (Num->r)->[r]->[Num];
    mapError3 = map;;
    
    
--- This should yield a "too specific" error
let
    onesError1: [Char];
    onesError1 = ones;;

--- As maperror    
let
    mapError2: (Num->r)->[q]->[r];
    mapError2 = map;;

    








