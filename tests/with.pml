type A = {x:Num};
type B = {y:Num};

let t1 = \ ( (x:A) -> x.x ) ;;
let t2 = \ ( ((x:A), (y:B)) -> x.x + y.y ) ;;
let t3 = \ ( ((x:A), (y:B)) -> x with y ) ;;

type C = A with B with { z:Num } ;;
let t4 (x: C) = x;;

do t3 ({x=3; y=4; z=5 }, {x=3; y=4; z=5 });
do t4 {x=3; y=4; z=5 };
