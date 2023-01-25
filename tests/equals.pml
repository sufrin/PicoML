
 
let rec ones = 1::ones;;
let yes = [1,1]=hd ones::hd(tl ones)::Nil;;
let no  = [2,1]=hd ones::hd(tl ones)::Nil;;
let no' = [1,2]=hd ones::hd(tl ones)::Nil;;

do "******* The following list should be all True";
do [yes =  True, no  =  False, no' =  False];
