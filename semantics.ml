(*
        
        Semantics
*)

open Syntax
open Mapping
open Utils
open Number


exception Exception of string
let except s = raise(Exception s)


type value = 
|    NumVal     of num
|    CharVal    of Utf8string.unicodechar
|    BoolVal    of bool
|    FunVal     of environment * identifier * expr
|    StructVal  of environment
|    CaseFunVal of environment * (expr (*past*) * expr) list * location
|    Delay      of expr_or_value ref
|    Strict     of (value -> value)
|    Lazy       of (value -> value)
|    Nil
|    Cons       of value * value
|    PairVal    of value * value
|    UnitVal 
|    Inject     of string * value
|    Const      of string
|    Opaque     of string *
                   [ `InChan  of in_channel
                   | `OutChan of out_channel 
                   | `Num of num
                   ]

and  environment = value map ref

and  expr_or_value  = 
|    Expression of environment * expr 
|    Value      of value
|    Generator  of (unit -> value)  (* used only for streaming files, etc *)

let bstring : value map -> string =      
    fun env ->  
    String.concat ", " (List.map (fun (i, e) -> (istring i)) env)

exception PatFail of expr * value

(*
        \subsection*{Override a value environment variable} 
*)

let (<+>): environment -> value map -> environment =
fun env map -> ref(!env |+| map)

(*
        \subsection*{Desugar an ascii string constant} 
*)

let eofcode = Uchar.of_int 4
let eof     = CharVal eofcode (* ctrl-d *)


let listify: bool -> (unit -> Utf8string.unicodechar option) -> value  = 
fun online gen -> 
    let rec next() =
            match gen() with
            |  Some ch ->
                if online && ch=eofcode then 
                   (Utils.echo true; Nil)
                else
                   Cons(CharVal ch, Delay(ref (Generator next)))
            | None -> Nil
    in  Delay(ref (Generator next))                  


let mkString: string -> value = 
fun s ->
let nextunicode = Utf8string.from_string s in
    listify false nextunicode

(*      
        \subsection*{Performance measurement} 
*)

let appcount   = ref 0
and forcecount = ref 0 
and sharecount = ref 0
and conscount  = ref 0
and starttime  = ref 0.0
let resetCounters() =
(
    appcount:= 0;
    forcecount := 0;
    sharecount:= 0;
    conscount:= 0;
    starttime := Sys.time()
)

let getCounters() =
    !appcount, !forcecount, !sharecount, !conscount,  Sys.time() -. !starttime

(*      
        \subsection*{Expression Evaluation}
*)

let rec eval: environment -> expr -> value = fun env -> fun e -> match e with
|   At (e', loc)       -> (try evalAt loc env e' with 
                          |  Failure s -> failwith (s^"\n in "^estring e)
                          |  Unbound s -> failwith ("Unbound "^s^"\n in "^estring e)
                          )
|  e' -> evalAt nowhere env e'

and evalAt = fun loc env e -> match e with
|   Unit               -> UnitVal
|   Hole   i           -> failwith ("Holes aren't bound to values "^estring e)
|   Id     i           -> env <?> i
|   ConId  i           -> env <?> i
|   Num    n           -> NumVal n
|   String s           -> force(mkString s)
|   Char   s           -> CharVal s
|   Pair   (e1, e2)    -> PairVal(delay env e1, delay env e2)
|   With   (e1, e2)    -> begin match (stricteval env e1, stricteval env e2) with
                                | StructVal m1, StructVal m2 -> StructVal (ref(!m1 |+++| !m2))
                                | other -> failwith ("dynamic type error in expression: "^estring e)
                          end
|   Fun    (bv, body)  -> FunVal(env, bv, body)
|   CaseFun cases      -> CaseFunVal(env, cases, loc)
|   Let    (dec, body) -> let env' = dval env dec in eval (env <+> env') body
|   Struct dec         -> let env' = dval env dec in StructVal (ref env')
|   If     (b, e1, e2) ->
    let br = match stricteval env b with 
    | BoolVal true  -> e1
    | BoolVal false -> e2
    | other         -> failwith ("dynamic type error in condition of: "^estring e)
    in
      eval env br
|   Select(s, field) ->
    ( match stricteval env s with
    | StructVal env' -> env' <?> field
    | other -> failwith ("dynamic type error in selector expression: "^estring e)
    )
|   Inside(s, body) ->
    ( match stricteval env s with
    | StructVal env' -> eval (env <+> !env') body
    | other -> failwith ("dynamic type error in open structure expression: "^estring e)
    )
|   Apply (f, e) ->
    let locate f x = 
    ( try f x with
      | Exception message -> raise(Exception (Format.sprintf "%s at %s" message (lstring loc)))
      | Failure message   -> raise(Failure (Format.sprintf "%s at %s" message (lstring loc)))
    ) in
    ( incr appcount;
      match stricteval env f with
      | FunVal(env', bv, body)  -> eval (env' <+> (bv|->delay env e)) body
      | Strict     ocamlfun     -> locate ocamlfun (stricteval env e)  
      | Lazy       ocamlfun     -> locate ocamlfun (delay env e) 
      | CaseFunVal(env', cases, loc) -> firstCase loc env' (delay env e) cases  
      | other -> failwith ("dynamic type error in application: "^estring f)
    )
|   At _      -> eval env e  (* parenthesised expressions *)
|   Has(e, _) -> eval env e  (* annotated expressions *)
    
and stricteval: environment -> expr -> value = 
fun env expr -> force(eval env expr) 

and force: value -> value = 
function 
| Delay (expr) -> begin 
    incr forcecount;
    match !expr with
    | Value     v -> incr sharecount; v
    | Generator f -> let v = f() in expr := Value v; v
    | Expression (env', e) -> 
      let v = stricteval (env') e in 
          expr := Value v;
          v
  end
| value -> value

    
and dval: environment -> decl -> value map = 
fun env -> function
|   ValDec   (pat, e) -> patMatch env pat (delay env e)
|   AndDec   (d1, d2) -> dval env d1 |+| dval env d2
|   SeqDec   (d1, d2) -> let env' = dval env d1 in env' |+| dval (env <+> env') d2 
|   WhereDec (d1, d2) -> let env' = dval env d2 in dval (env <+> env') d1 
|   HasType  _        -> []
|   RecDec d -> 
    let env' = ref(!env) 
    in  let ext  = dval env' d 
        in  env' := !env |+| ext;
            ext

and patMatch: environment -> expr (* pattern *) -> value -> value map =
fun env -> fun pat -> fun v  -> match stripAt pat with
|   ConId "True"  -> 
    (match force v with 
      | BoolVal true  -> emptymap
      | other -> raise (PatFail(pat, other))
    )
|   ConId "False"  -> 
    (match force v with 
      | BoolVal false   -> emptymap
      | other -> raise (PatFail(pat, other))
    )
|   ConId "Nil"  -> 
    (match force v with 
      | Nil   -> emptymap
      | other -> raise (PatFail(pat, other))
    )
|   Num n -> 
    (match force v with 
      | NumVal n' when n =/ n'  -> emptymap
      | other                   -> raise (PatFail(pat, other))
    )
|   Char n -> 
    (match force v with 
      | CharVal n' when n=n'  -> emptymap
      | other                 -> raise (PatFail(pat, other))
    )
|   ConId id     -> 
    (match force v with 
    |  Const id' when id=id' -> emptymap
    |  other                 -> raise (PatFail(pat, other))
    )
|   Hole _       -> emptymap
|   Id id        -> id |-> v
|   Pair(p1, p2) -> 
    (match force v with
         (* Pattern matching is linear: hence the use of disjoint mapping sum.
            With a bit more work it could be constrained.
         *)
     |   PairVal(v1, v2) -> patMatch env p1 v1 |++| patMatch env p2 v2
     |   other           -> raise (PatFail(pat, other))
    )
|   Apply(ConId "::", Pair(hpat, tpat)) ->
    ( match force v with
    | Cons(hv, tv) -> patMatch env hpat hv |++| patMatch env tpat tv
    | other -> raise (PatFail(pat, other))
    )
|   Apply(ConId id, pat) -> 
    (match force v with 
    | Inject(id', v) when id=id'  -> patMatch env pat v
    | other -> raise (PatFail(pat, other))
    )
|   Unit  -> 
    (match force v with 
    |  UnitVal  -> emptymap 
    |  other    -> raise (PatFail(pat, other))
    )
|   String s -> if  s=force_string (force v) then emptymap else raise (PatFail(pat, v))
|   Has(pat, _) -> patMatch env pat v
|   other       -> failwith("Invalid pattern: "^estring pat)

and firstCase: location ->  environment -> value -> (expr (* pat *) * expr) list -> value =
fun loc -> fun env -> fun v -> fun cases -> 
    try findCase env v cases with
    | Failure s -> failwith (s^" in case function defined at "^lstring loc)

and findCase env v = function 
| (pat, body) :: rest -> 
    ( try let env' = patMatch env pat v in 
          let benv = env <+> env' in
              eval benv body 
      with
      |   PatFail _ -> findCase env v  rest
    )
| [] -> failwith("Out of cases")

and (<?>): environment -> string -> value = (* used only in debugging *)
fun env id -> 
    try  !env |?| id 
    with exn -> 
        Format.printf "Unbound variable %s in environment:\n" id;
        printenv (!env);
        raise exn

and force_string: value -> string = 
fun plist ->
    let buf = Buffer.create 120 in
    let rec append = function
    |  Cons(h, t) -> 
       ( match force h with
       | CharVal c -> Buffer.add_utf_8_uchar buf c; append (force t)
       | v         -> print_approx_val v; failwith "Non-string"
       )
    |  Nil  -> ()
    |  v -> print_approx_val v; failwith "Non-cons"
    in append (force plist);
       Buffer.contents buf

(* 
        \subsection*{Delaying the evaluation of a parameter or definiens}
*)

and  delay: environment -> expr -> value = fun env expr -> 
match expr with
| Fun   _ 
| CaseFun  _ 
| ConId _ 
| Hole  _ 
| Id    _ 
| Num   _ 
| Char  _ -> eval env expr
| _       -> Delay(ref(Expression(env, expr)))

(*
        \subsection*{Top-level printing}
*)

and printenv env =  
    Mapping.iterpairs (fun i v -> (Format.printf "%s = " i;  print_approx_val v; Format.printf "\n"; Format.print_flush())) env

(* For debugging *)
and print_approx_val = function
| Cons (v, vs) -> 
   Format.print_string "(::)";
   print_approx_val(v);
   Format.print_string " ";
   print_approx_val(vs);
   Format.print_string ")";
| PairVal(v1, v2) ->
   Format.print_string "(";
   print_approx_val(v1);
   Format.print_string ",";
   print_approx_val(v2);
   Format.print_string ")"
| Inject(name, v) ->
   Format.print_string name;
   Format.print_string " ";
   print_approx_val v;
   Format.print_string ")"
| Delay (expr) -> begin
    Format.print_string "<delay: ";
    match !expr with
    | Value     v -> print_val v
    | Generator f -> Format.printf("<generator>")
    | Expression (env', e) -> Format.printf "%s ((with bindings for %s))" (estring e) (bstring (!env'))
    ;
    Format.print_string ">";
    Format.print_flush() 
  end
| other -> print_val other

and print_val = function
| StructVal env -> Format.print_string "{";
                   Mapping.interiorpairs
                   (fun interior i v -> 
                    (Format.printf "%s=" i;  
                     Format.print_flush();
                     print_val (force v); 
                     if interior then Format.printf "; "; 
                     Format.print_flush())) !env;
                   Format.print_string "}"; 
                   Format.print_flush()
| Cons (v, vs) as list -> 
  (match force v with
  | CharVal _ -> Format.print_string "\"";    
                 Format.print_flush();
                 print_string list;
                 Format.print_string "\""
  | other     -> print_list list
  )
| PairVal(v1, v2) ->
   Format.print_string "(";
   Format.print_flush();
   print_val(force v1);
   Format.print_string ",";
   Format.print_flush();
   print_sndval(force v2);
   Format.print_string ")"
| Inject(name, v) ->
   if (isOp name) then
      print_infval name (force v)
   else 
   let par = is_parenthesised v in
      Format.print_string name;
      Format.print_flush();
      print_parenthesised (not par) (force v)
| Const name ->
   Format.print_string name

| NumVal  i         -> Format.print_string (string_of_num i)
| CharVal c         -> Format.printf "'%s'" (Utf8string.to_utf8string c)
| BoolVal true      -> Format.print_string "True"
| BoolVal false     -> Format.print_string "False"
| Delay   _         -> Format.print_string "<delay>"
| FunVal  _         -> Format.print_string "<function>"
| CaseFunVal(_, cases, _) -> Format.print_string "<casefunction>";
                                    
| Strict _          -> Format.print_string "<built-in(strict)>"
| Lazy _            -> Format.print_string "<built-in(lazy)>"
| Nil               -> Format.print_string "[]"
| UnitVal           -> Format.print_string "()"
| Opaque (hint, _)  -> Format.printf "<opaque> (%s)@." hint

(* Print a list of characters without quotes or commas. *)
and print_string  = function
|  Cons(h, t) -> 
   ( match force h with
   | CharVal c -> Format.print_string  (Utf8string.to_utf8string c)
   | _         -> failwith "non-character in character-list"
   );
   Format.print_flush();
   print_string (force t)
|  Nil  -> ()
|  _ -> failwith "non-list as tail of cons"

and print_list  = function
|  Cons(h, t) -> 
   Format.print_string "[";
   Format.print_flush();
   print_val(force h); 
   Format.print_flush();
   print_list_tail (force t);
   Format.print_string "]";
   Format.print_flush();
|  Nil -> Format.print_string "[]"

and print_list_tail = function
|  Cons(h, t) -> 
   Format.print_string ", "; 
   Format.print_flush();
   print_val(force h); 
   Format.print_flush();
   print_list_tail (force t);
   Format.print_flush();
|  Nil -> ()
|  _ -> failwith "non-list as tail of cons"

and print_sndval = function
| PairVal(v1, v2) ->
   print_val(force v1);
   Format.print_string ",";
   Format.print_flush();
   print_sndval(force v2)
| other -> print_val other

(* 
        TODO: fix to use priorities to determine parenthesised printing
*)
and print_infval op = function
| PairVal (l, r)  ->
   Format.printf "(";
   print_val (force l);
   Format.printf " %s " op;
   Format.print_flush();
   print_val (force r);
   Format.printf ")";
| other -> print_val other

and is_infix e =
match force e with
| Inject(name, _) -> isOp name
| Cons _          -> true         
| other           -> false

and is_parenthesised e = 
match force e with
| Inject(name, _) -> isOp name
| Cons _          -> true         
| other           -> false

and print_parenthesised p e =
   if p then Format.print_string "(";
   Format.print_flush();
   print_val (force e);
   if p then Format.print_string ")";
   Format.print_flush()

        

































































