let 

        rec fix f = f(fix f);
               

        factorial = fix ( λ fac n ->  if n=0 then 1 else n*fac(n-1))
;;


do factorial 50;


