/*
        Illustrating data types, infix constructors, the right associative 
        application operator, $, used in patterns, and anonymous functions
        defined by cases (flatten). A sequential declaration can be followed
        by a where clause that makes definitions whose scope is the sequence.
        
*/

notation rightdata ^^; --- ^^ is an right associative infix constructor 

data Tree a = E | T a | (Tree a) ^^ (Tree a);

let atom  $ T a = a
and left  $ l ^^ r = l
and right $ l ^^ r = r;;

let rec flatten =
    (
    | E        -> []
    | T t      -> [t]
    | l ^^ r   -> flatten l ++ flatten r
    );;

let x     = T 1 + E + T 2 + T 3;
    y     = x + T 4;
    rec z = x + y + z
    where 
    (+) = (^^);;
    






