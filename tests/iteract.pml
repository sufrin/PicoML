/*
        Benchmark for action performance (space and time).
        Each iter1, iter2 prints something every 20K actions.
*/


let iter m = 
    put m        >> 
    putStrLn "*" >> 
    iter (m+1);;

let iter1 (n, m) = 
    if m=50 then return () else
    if n=1 then put m >> putStrLn "*" >> iter1 (20000, m+1) else iter1(n-1, m);;

let iter2 n m = 
    if m=50 then return () else
    if n=1 then put m >> putStrLn "*" >> iter2 20000 (m+1) else iter2 (n-1) (m);;

do iter1 (1,0);
do iter2 1 0;




