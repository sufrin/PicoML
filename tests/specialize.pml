--- Declaration can be a specialization of inferred type
--- (see also erroneous.pml)

let map: (Num->a) -> [Num] -> [a];
    rec map f = λ( []->[] | x::xs->f x :: map f xs );;

