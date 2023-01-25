/*
        A few deliberate type errors to check contextual reporting.
*/
let 
    rec take n xs =
        if null xs then [] else 
        if n=0     then [] else
           hd xs + take (n-1) (tl xs) ;;  

let    
    rec (++) = \( ([], ys) -> ys
                | (x::xs, ys) -> x::(xs+ys) 
                );;

let x::xs=3;;

let _ = let a=3; b=3; a=4; b=4 in a+b;;

let _ = let a=3 and b=3 and a=4 and b=4 in a+b;;

let    rec (++) = \( (0, ys) -> ys
                   | (x::xs, ys) -> x::(xs++ys) 
                   );;

let _ = 
    let a: [Num]; a=3 in a;;

let _ = 
    let a: [Num]; a=3 in a;;

let a: [Num]; a x=3;
    b: Num; b='c'
    ;;

let    
    (++) = \( ([], ys) -> ys
               | (x::xs, ys) -> x::(xs++ys) 
               );
    double l = l++l
    ;;
    
let _ = let a="foo" in double 4;;

let _ = let a="foo" in double b;;

let _ = let a:X and b()=() in double b;;

