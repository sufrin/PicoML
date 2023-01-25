(*      $Id: env.ml 177 2019-08-20 17:48:19Z sufrin $ *)

(*

        This module defines type environments, type substitution,
        polytype instantiation, and polytype standardisation.

*)

open Type
open Utils
open Syntax
open Mapping

type env = typexpr map

let emptyenv : env = []

(* \subsubsection*{Convert to human-readable form} *)

let envstring : env -> string =      
    fun env ->  
    String.concat "\n" (List.map (fun (i, t) -> (istring i)^": "^tstring t) env)
    
    
let unknownsInEnv : env -> typexpr list =
    fun env -> 
     (typeUnion (List.map (fun (i, t) -> unknownsInType t) env))


(* \subsubsection*{Instantiation of a polymorphic type} *)

(*      $substitute [ id |-> ty; ...] t$ substitutes $ty, ...$ for all occurences
        of the type variable $id, ...$ in $t$
*)

let rec substitute : env -> typexpr -> typexpr =
    fun env t ->
    match t with
    |  VarType i -> (try env |?| i with Unbound _ -> failwith ("Free Type Variable: "^i^" in polytype body."))
    |  PairType(t1, t2)  -> PairType (substitute env t1, substitute env t2)
    |  FunType (t1, t2)  -> FunType  (substitute env t1, substitute env t2)
    |  ListType t        -> ListType (substitute env t)
    |  ConType(n, ts)    -> ConType  (n, List.map (substitute env) ts)
    |  StructType env'   -> StructType  (Mapping.map (substitute env) env')
    |  ExpandedType(t1, t2)-> ExpandedType(substitute env t1, substitute env t2)
    |  _                 -> t

(*      $substitution [id1; ...]$ constructs an environment suitable
        for use by substitute, in which $id1, id2, ...$ are each bound
        to new type unknowns.
*)

let substitution: string list -> env =
fun vs -> Mapping.tabulate newUnknown vs        

(*      
        $instantiate$ makes an instance of its argument, with
        new unknowns substituted for the variables (if any) over which
        it is abstracted.
*)
let instantiate : typexpr -> typexpr =
function (PolyType (vs, t)) -> substitute (substitution vs) t
|        t                  -> t
    

(* Standardising a polytype changes the names of its type variables 
   so that the first is \verb$`a'$ the second \verb$`b'$, and so on. 
*)

let newVarName: int ref -> 'a -> identifier = fun counter _ ->
let r = Pointer.makename(!counter) in incr counter; r

let standardise: typexpr -> typexpr = function
|   PolyType(_, body) ->
    let params  = List.rev @@ varNamesInType body (* to alphabetise names sensibly *)
    in 
    let counter = ref 0
    in
    let params' = List.map (newVarName counter) params
    in
    let substitution: env = 
        Mapping.fromLists params (List.map mkVarType params')
    in  
        PolyType(params', substitute substitution body)
    
|   other -> typeclass other


(* \subsubsection*{Equality of standardised types} *)

let rec sametype: typexpr -> typexpr -> bool =
fun ty1 ty2 ->
match expand @@ typeclass ty1, expand @@ typeclass ty2 with
|   UnitType,           UnitType              -> true
|   NumType,            NumType              -> true
|   BoolType,           BoolType             -> true
|   CharType,           CharType             -> true
|   ListType t,         ListType t'          -> sametype t t'
|   ConType(n, ts),     ConType(n', ts')     -> n=n' && List.for_all2 sametype ts ts'
|   PairType(t1, t2),   PairType(t1', t2')   -> sametype t1 t1' && sametype t2 t2'
|   FunType (t1, t2),   FunType (t1', t2')   -> sametype t1 t1' && sametype t2 t2'
|   PolyType(vs1, ty1), PolyType(vs2, ty2)   -> sametype ty1 ty2
|   VarType v,          VarType w when v=w   -> true
|   StructType env,     StructType env'      ->
    List.for_all (fun (i, t) -> (sametype t (env' |?| i))) env &&
    List.for_all (fun (i, t) -> (sametype t (env |?| i))) env'   
|   _,                  _                    -> false






















