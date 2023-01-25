--- 
--- Tests the use of ExpandedType more pervasively while tracng/reporting
---

type A x = x and B x = A x;

let f:B x->A x; f x=x;;

do f 3;

let curry f x y = f(x,y);;

do f(curry (+));

type Int=Num;

let g:Int->Int; g x=x;;

do g 3;

data C x = C x | D;

let h:C a -> a; h(C x)=x;;




 
