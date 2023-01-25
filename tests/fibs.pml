---
--- linear order 2 recurrences via exponentiation of pairs
--- >< is commutative and associative with unit (0,1)
--- 
    
    (a,b) >< (c,d) = ((a*c) + (a*d) + (b*c), (a*c) + (b*d));
    
    unit = (0, 1);
    
    --- linear time pair exponentiation
    pair *** n = let f = \( 0 -> unit | n -> pair >< f(n-1) ) in f n;
    
    --- logarithmic time general exponentiation k^n
    pow (><) unit k0 n0 = let f mul result = 
                                --- mul >< result^n = k0^n0
                                \( 0 -> unit
                                 | 1 -> mul >< result
                                 | n -> let (n', m) = divmod(n/2) in
                                        let square = result >< result in
                                        if    m=1 
                                        then  f (mul >< result) square n' 
                                        else  f mul             square n'
                                 ) in f unit k0 n0;
     
     --- natural exponentiation
     exp  = pow  (*) 1;
     
     --- fibs by pair exponentiation
     fib2 = pow (><) unit (1,0)
    
    

