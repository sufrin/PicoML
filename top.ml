(*
        Processing top-level phrases
*)

open Type
open Infer
open Parsing
open Env
open Mapping
open Syntax
open Trace


let print_expr : expr -> unit =
    fun e -> Format.print_string (estring e)

let print_type : typexpr -> unit =
    fun t -> Format.print_string(tstring t)
    
let print_type_env : env -> unit =
    List.iter (fun (i, t)-> 
               Format.print_string(istring i^":\t"^tstring t);
               Format.print_newline())

let print_decl : decl -> unit =
    fun d -> Format.print_string (dstring d)
    

let astring: alt -> string =
    fun a ->
    match a with
      (id, None) -> id
    | (id, Some t) -> id^" "^tstring t

let print_data (id, params, alts) =
    Format.print_string id;
    (match params with
      [] -> ()
    | _  -> Format.print_string ("("^(String.concat ", " params)^")"));
    Format.print_string "=";
    Format.print_string (String.concat " | " (List.map astring alts))

    

(*
        We typecheck an expression by calculating its type,
        and a declaration by adding the type environment extension it
        generates to the top-level type environment. 
     
        Semantic processing follows typechecking.
*)

let typeEnv : env ref = Builtin.typeEnv

let typeDefs : env ref = Builtin.typeDefs (* maps TYPE ids to type expressions *)

let validTypes : typexpr list ref = ref[]

let typeCheck : bool ref = ref true
let typeShow  : bool ref = ref true

open Semantics
open Builtin
open Datatype

let print_env : env -> value map -> unit =
fun typenv valuenv -> 
    Mapping.iterpairs 
       (fun i t -> if not (Syntax.holeId i) then Format.printf "%s : %s@." (istring i) (tstring t))
       typenv
       

let printCounters() =
    let apps, forces, shares, conses, elapsed = getCounters() in
        if elapsed > 1.0e-3 
        then Format.printf "\n(%g secs, " elapsed
        else Format.printf "\n(<1 msec, ";
        Format.printf "%d/%d shared, %d apps, %d cons)@." shares forces apps conses

let rec check_and_eval : phrase -> unit = 
fun phrase ->
match phrase with
|  Located(phrase', loc) ->
     (try check_and_eval_simple phrase' with
        | TypeError (None, message, env) -> typeErrorAt loc message env
     ) 
| phrase' -> check_and_eval_simple phrase'
     
and check_and_eval_simple: phrase -> unit = 
fun phrase -> match phrase with 
|  Expr e -> 
   if !typeCheck then
      let ty     = etype (!typeDefs) (!typeEnv) e in
      let string = sametype ty @@ ListType CharType
      and unit   = sametype ty UnitType
      and action = match typeclass ty with 
                   |  ConType("ACT", [t]) when sametype (typeclass t) UnitType -> 1    
                   |  ConType("ACT", ts)             -> 2   
                   |  _                                 -> 0    
      and value  = (resetCounters(); stricteval valueEnv e)
      in  (* 
              RESULTS OF EVALUATION ARE TREATED AS FOLLOWS:
                 
                 A value of type ACT() gets executed, and its unit value is discarded
                 A value of type ACT t gets executed, and its result (of type t) may be printed, 
                   along with its type
                 
                 A value of type () is ignored 
                 A value of type [Char] is printed unquoted
                 A value of any other type, t, gets printed and t itself may be printed along with
                   a short report on the resources used in the evaluation.
          *)
          if action == 1 then
             ignore (Action.performAct value)
          else
          if action==2 then
          (
             print_val (force (Action.performAct value));
             if !typeShow then 
             ( 
                Format.print_string " : "; 
                print_type ty
             );
             Format.printf "@."
          )
          else
          if not unit then begin
             (if string then print_string else print_val)(value);
             Format.printf "@."
          end;
          if !typeShow && not string && not unit &&  action = 0 then 
          (  Format.print_string " : "; 
             print_type ty;
             printCounters()
          )
          else
          ()
      else
          (* After evaluation without type-checking, the resulting value is printed *)
          print_val( stricteval valueEnv e )
      
| Decl d -> 
  if !typeCheck then
     let env'  = declTypes true (!typeDefs) (!typeEnv) d  
     in  
     let venv' = dval valueEnv d 
     in  valueEnv := !valueEnv |+| venv';
         if !typeShow then print_env env' venv';
         typeEnv := simplify(!typeEnv |+| env')
  else
     let venv' = dval valueEnv d 
     in  valueEnv := !valueEnv |+| venv'

| Eof -> ()
                
| Data  decls -> 
  let defs', env', venv' = declareData (!typeDefs) (!typeEnv) decls 
  in  valueEnv := !valueEnv |+| venv';
      if !typeShow then print_env env' venv';
      if !typeShow then print_defs defs';
      typeEnv := simplify(!typeEnv |+| env');
      typeDefs := simplify(!typeDefs |+| defs')

| Type  decls -> 
  let defs' = declareType (!typeDefs) decls 
  in  
      if !typeShow then print_defs defs';
      typeDefs := simplify(!typeDefs |+| defs')
      
| Error s -> Format.printf "Syntax error at %s\n@." s



let process phrase = 
    reset(); 
    resetindent();
    Syntax.clearPath();
    Syntax.setIsOp(Scanner.isOp);
    (let handler s = raise (Failure (Format.sprintf "Signal %d" s))
     in
     let open Sys in
        catch_break true;
        set_signal (sigsegv) @@ Signal_handle handler
    );
    
    (try check_and_eval phrase with 
    |    Stack_overflow      ->  Format.print_string ("\nStack overflow"); 
                                 printCounters();
                                 Format.print_newline()
    |    Exception  message  ->  Format.print_string ("\nException: "^message); 
                                 printCounters();
                                 Format.print_newline()
    |    Failure     message  -> Format.print_string ("\nError: "^message); 
                                 printCounters();
                                 Format.print_newline()
    |    TypeError(None, message, env) -> 
                                 Format.printf "Type error: %s" message;
                                 Format.print_newline()
    |    TypeError(Some loc, message, env) -> 
                                 Format.printf "Type error at %s: %s" (lstring loc) message;
                                 Format.print_newline()
    |    NonDisjoint message  -> Format.print_string ("Declaration error: multiple declaration of "^message);
                                 Format.print_newline()
    |    Unbound   id         -> Format.print_string ("Unbound variable: "^id);
                                 Format.print_newline()
    |    Action.IOException v -> Format.print_string "\nIOException: "; 
                                 print_val(force v); 
                                 Format.print_newline()
    |    Invalid_argument s   -> Format.eprintf "Bug in picoml: invalid argument to %s@." s
    |    Sys.Break            -> Format.eprintf ("\n[Interrupted]@.")
    |    exn                  -> Format.eprintf "Bug in picoml: unexpected exception %s@." (Printexc.to_string exn)
    );
    Format.print_flush();
    Utils.echo(true)










