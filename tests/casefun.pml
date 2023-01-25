---
---  There are several notations for case functions
---
---  λ( cases ) 
---  \( cases )
---  (| cases )
---  (| cases |)
---  ⟨ cases  ⟩
---
let rec len n  = λ( []    -> n 
                  | x::xs -> len (n+1) xs 
                  );;
                 
let rec length = (
                 | (n, [])    -> n 
                 | (n, x::xs) -> length (n+1, xs) 
                 );;

let l xs =  len 0 xs;;

let invert = (| 0->1 | 1->0 |);;

--- a type error
do "**** A type error should follow";
let rec span   = (
                 | (x, y) -> span y + x 
                 | other  -> 0 
                 );; --- cannot be consistently typed


