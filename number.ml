(*
        Number representation
        
        It would be straightforward to add a plethora of representations here, but
        we have limited pourselves just to unbounded-precision rationals.
        
*)



type num = Q.t
module Local = struct
 
 let splat: string -> char -> string * string =
  fun s c -> 
  let p = String.rindex s c in (String.sub s 0 p, String.sub s (p+1) (String.length s-p-1))

 let isPositive n = n>=0 (* Warning: there is no easier way of doing this in 4.11 *)
 
 open Q
 open String
 
 module Z = Big_int_Z
 type _Z_ = Z.big_int
 
 let ( // )=div
 let ( */ )=mul
 let ( -/ )=sub
 let ( +/ )=add
 
 let ten: _Z_    = Z.big_int_of_string "10"
 let one: _Z_    = Z.unit_big_int
 let negone: _Z_ = Z.minus_big_int one
 

 let pow10: int -> _Z_ = 
     fun exp -> Z.power_big_int_positive_int ten exp
 
 let qpow10: int -> num = 
         fun exp -> 
             if isPositive exp  then 
                Q.make (pow10(exp)) one
             else
                Q.make one (pow10(Pervasives.abs exp)) 
 

  let fraction s = Q.make (Z.big_int_of_string s) (pow10 (String.length s))
  let exponent s = qpow10 (int_of_string s)



let rec num_of_string : string -> num = Q.of_string

(*
  fun s -> 
  try let (n, f) = splat s 'e' in num_of_string n */ exponent f
  with
  Not_found -> 
  try let (n, f) = splat s 'E' in num_of_string n */ exponent f
  with
  Not_found -> 
  try let (n, f) = splat s '.' in num_of_string n +/ fraction f
  with
  Not_found -> Q.of_string s
*)
    
  let prec = ref 0
  let pr   = ref Q.to_string 
  
  let string_of_num n =  !pr n
  
  let set_print_precision n =
     prec:= Q.to_int n;
     match !prec with
     |  0 -> pr := Q.to_string
     |  n -> pr := (fun q -> Float.to_string(Q.to_float q))
            (* WAS:
             if n>0 then
                pr := approx_num_fix n
             else
                pr := approx_num_exp (-n)
             *)
   
   let floor: num -> num =
       fun n ->
       let num = n.num
       and den = n.den
       in Q.make (Z.div_big_int num den) one
   
   let integer: num -> num =
       fun n ->
       let num = Z.abs_big_int n.num
       and den = Z.abs_big_int n.den
       in 
         match sign n with
         | 0    ->Q.zero
         | 1    ->Q.make (Z.div_big_int num den) one
         | (-1) ->Q.make (Z.div_big_int num den) negone
       
       
   let numden: num -> (num*num) =
       fun n ->
       let num = Q.make n.num one
       and den = Q.make n.den one
       in (num, den)
   
   let divmod: num -> (num*num) =
       fun n ->
       let q, r = Z.quomod_big_int n.num n.den
       in (Q.make q one, Q.make r one)
       
   let is_int: num -> bool = fun n -> Z.eq_big_int n.den one
       
   let pow: num -> num -> num =
       fun x exp ->
       if Z.eq_big_int (den exp) one then
          let exp' = Z.abs_big_int exp.num in
          let n' = Z.power_big x.num exp'
          and d' = Z.power_big x.den exp'
          in  if Q.sign(exp) == -1  then Q.make d' n' else Q.make n' d'
       else
          failwith "exponentiation with fractional power"
       
   
  
end
let num_of_string:       string -> num    = Local.num_of_string;;
let string_of_num:       num    -> string = Local.string_of_num;;
let set_print_precision: num    -> unit   = Local.set_print_precision;;
let zero: num                             = Q.zero;;
 let ( // )=Q.div;;
 let ( */ )=Q.mul;;
 let ( -/ )=Q.sub;;
 let ( +/ )=Q.add;;
 let ( =/ )=Q.equal;;
 let ( </ )=Q.lt;;
 let ( >/ )=Q.gt;;
 let ( <=/ )=Q.leq;;
 let ( >=/ )=Q.geq;;
 let ( **/ )=Local.pow
 let floor_num=Local.floor;;
 let numden_num=Local.numden;;
 let divmod_num=Local.divmod;;
 let integer_num=Local.integer;;
 let is_int_num=Local.is_int;;










