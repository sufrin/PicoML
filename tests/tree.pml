notation rightdata :+: :+ ;;

data T a = N | L a | (T a)  :+: (T a);;

let rec ttake n = λ( N   -> N
                   | L a -> L a
                   | l :+: r -> if n=0 then N else ttake (n-1) l :+: ttake (n-1) r 
                   );;
                  
                  
let x = N :+: N
and y = L 1 :+: L 1
and z = L 2 :+: L 3 :+: L 4; inf = z :+: inf;; --- ... and inf ... won't do here

data List a = a :+ (List a) | E;;

let k1 = 1 :+ 2 :+ 3 :+ E;;

let t1 = L k1 :+: L k1;;



