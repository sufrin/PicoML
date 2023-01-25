
let rec remdups : 'a list -> 'a list = function 
    |        []      -> []
    |        (x::xs) -> if List.mem x xs then remdups xs else x::remdups xs;;
    
let union xs ys = remdups (xs@ys);;

let distunion xss = List.fold_right union xss [];;
    
let rec (--) : 'a list -> 'a list -> 'a list =
    fun xs ys ->
       List.fold_right 
        (fun x zs -> if List.mem x ys then zs else x::zs) xs [];;
        
let rec filter : ('a -> bool)  -> 'a list-> 'a list =
    fun p xs -> 
    match xs with 
    |        []      -> [] 
    |        (x::xs) -> if p x then x::filter p xs else filter p xs;;

let intersection : 'a list -> 'a list -> 'a list =
     fun xs ys -> filter (fun x -> List.mem x ys) xs;;
     

(* 
        Assume that picoml starts in the mode we wish to be restored 
        when we turn echoing back on again.
*)

let stdinattr: Unix.terminal_io option ref = ref None

let echo echoing =
    if Unix.isatty Unix.stdin then
       if echoing then
          match !stdinattr with
          | Some attr -> Unix.tcsetattr Unix.stdin Unix.TCSANOW attr
          | None -> ()
       else 
       let initial = Unix.tcgetattr (Unix.stdin)
       and attr = Unix.tcgetattr Unix.stdin
       in  stdinattr := Some initial;
           attr.Unix.c_echo   <- false;
           attr.Unix.c_icanon <- false;
           attr.Unix.c_ixon   <- false;
           attr.Unix.c_echonl <- false;
           attr.Unix.c_vmin   <- 1;
           attr.Unix.c_vtime  <- 0;
           Unix.tcsetattr Unix.stdin Unix.TCSANOW attr
     else ()


(*
    Implementation for \verb"#use" phrases in the parser
*)

    let fileuse: (string->unit) ref  =  ref(fun _->())
    let usefile s = !fileuse s

(* Turn infixed id into its name *)
let stripslosh s = if s.[0]='`' then "_"^String.sub s 1 (String.length s - 2) else s

(* Implementation of notation phrases in the parser *)
let declarenotations: ((string * string * string list) list -> unit) ref = ref(fun _ -> ())
let declareNotations location notations = 
    try !declarenotations notations with
       Failure msg -> Format.printf "Notation declaration error (%s) at: %s\n@." msg location

let setDeclareNotations action = declarenotations := action






















