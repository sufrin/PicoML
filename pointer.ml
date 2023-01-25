(* $Id: pointer.ml 31 2018-09-29 15:15:52Z sufrin $  *)
(*
        The type of pointer variables.

        An 'a pointer is identified by a (unique) integer, and is either
        set to an 'a or is unset. Once set it will never be unset.

        A pointer variable has a human-readable name which is easily
        derivable from its identity.

*)
type 'a pointer = int * 'a option ref;;
exception Value;;

let set: 'a pointer -> 'a -> unit =
    fun (_, r) v -> 
        r := Some v;;
        
let value: 'a pointer -> 'a =
    fun (_, r) -> 
    match !r with Some v -> v | _ -> raise Value;;

let isSet: 'a pointer -> bool = 
    fun (_, r) -> 
    match !r with
    |     None -> false 
    |     _    -> true;;
    
let same: 'a pointer -> 'a pointer -> bool =
    fun (i, _) (j, _) -> i=j;;

(*  Details of printnames for invented pointers *)
  
let alpha i = Char.chr(i+Char.code 'a')
let radix = 26
let radix2 = radix*radix
let radix3 = radix2*radix

(* The name of pointer i is represented as up to 3 digits in radix 26; otherwise something equally scrutable *)

let rec makename : int -> string = 
fun i -> if 0<=i && i<radix then String.make 1 (alpha(i)) else
         if radix<=i && i < radix2 then Format.sprintf "%c%c" (alpha(i/radix - 1)) (alpha(i mod radix)) else
         if radix2<=i && i < radix3 then Format.sprintf "%c%s" (alpha(i/radix2 - 1)) (makename (i mod radix2)) else
         Format.sprintf "a%d" i 

(* Tabulate the first 676 names: if any more appear regularly then I'll think again *)
let names = Array.init radix2 makename

let name: 'a pointer -> string =
    fun (i, _) -> if i<radix2 then Array.get names i else makename i;;
    

 











