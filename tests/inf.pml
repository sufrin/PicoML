/*
        Some tabulated functions
*/
let 
    nats: [Num];
    fibs: [Num];
    facs: [Num];

    rec nats = 0 :: map ⟨ x -> x+1 ⟩ nats;
    
    rec fibs = 0 :: 1 :: fibnext fibs;
    
    rec fib n = if n=0 then  0 else if n=1 then 1 else fib (n-1)+fib(n-2);    
    
    rec facs = 1 :: zipWith (*) facs (tl nats);
    
    rec fac n = if n=0 then 1 else n*fac(n-1)
   
    where 
    
    rec fibnext $ x::xs = let y::ys=xs in x+y::fibnext xs
    
end





