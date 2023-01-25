/*
        Newton: piteously slow 
*/
let rec iterate f x = x::iterate f (f x);;

let rec newtonStep f f' x = x - f x / f' x
    and sqrt x = 
        let step = newtonStep λ( r -> r*r - x ) (2*)
        in  iterate step (x/2)
    and cbrt x = 
        let step = newtonStep λ( r -> r*r*r - x ) λ( r-> 3*r*r )
        in  iterate step (x/2)
end


