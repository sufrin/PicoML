(*      
        $Id: trace.ml 21 2018-09-24 15:19:59Z sufrin $
*)
(* 
        In this section we define the machinery we will use within
        infer/type to trace the execution of the inference algorithm
*)

let ind = ref 0
let resetindent() = ind := 0
let inc() = ind:=1+ !ind; !ind
let dec() = ind:=!ind - 1; !ind

let tracing  = ref false

let trace s = 
    if !tracing then begin
       for i=1 to !ind do print_string(" ") done;
       print_string s;
       print_string "\n"
    end





