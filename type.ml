(* $Id: type.ml 177 2019-08-20 17:48:19Z sufrin $  *)


(*
        As type inference proceeds, the pointer fields of
        [Unknown] types are updated as a consequence of the
        equations between types which are discovered by the
        type inference procedure.

        Each unknown type is either completely unset, or has
        its pointer bound to a type expression to which it
        must be equivalent.

        The $typeclass$ of a type is the type itself (if it isn't
        an unknown) or is the representative of the class of
        all types to which this unknown has been deduced to
        be equal.
*)

open Pointer
open Trace
open Mapping

type identifier = string

type typexpr =
|   UnitType
|   NumType
|   BoolType
|   CharType
|   PairType     of typexpr * typexpr
|   WithType     of typexpr * typexpr
|   ListType     of typexpr
|   ConType      of string  * typexpr list
|   FunType      of typexpr * typexpr
|   StructType   of typexpr map
|   UnknownType  of typexpr pointer
|   VarType      of string
|   ExpandedType of typexpr * typexpr       (* source, expansion *)
|   TypeFunType  of string list * typexpr   (* Λ (v1, v2 ... vn) -> t *)
|   AbbrType     of string * typexpr list * typexpr   (* introduced by type foo (x, ...) = ... *)
|   PolyType     of string list * typexpr

let arity = function
| PolyType (vars, _) -> List.length vars
| _                  -> 0

let rec expand: typexpr -> typexpr = function
|   ExpandedType(_, exp) -> expand exp
|   other                -> other

let isPoly: typexpr -> bool = function
|   PolyType _ -> true
|   VarType _  -> true (* impossible: types with variables in are automatically promoted to polytypes by parser *)
|   _          -> false
(*
        This machinery is used to generate new type
        unknowns, and to compare and set type unknowns.
*)

let counter      = ref 0
let reset()      = counter := 0
let newUnknown _ =     
    let r = UnknownType(!counter, ref None)
    in  counter := !counter+1;
        r

let sameUnknown (UnknownType p) (UnknownType p') = Pointer.same p p'
let setUnknown  (UnknownType p) t = Pointer.set p t
let unknownName (UnknownType p)   = Pointer.name p

(*
        The class of a known type expression is the expression itself.
        The class of an unknown is (if it is unset) the unknown itself.
        The class of an unknown which has been set (by virtue of
        having been unified) to another type expression is the
        same as the class of that type expression -- even if that
        expression is (in turn) another unknown.
        
        The following function maps a type expression to its class,
        and (as an optimisation) shortens any paths from set unknowns
        to their class. The optimisation is logically unnecessary.
*)

let rec typeclass : typexpr -> typexpr =
fun t ->
match t with 
| UnknownType p when isSet p -> let t' = typeclass (value p) in set p t'; t'
| _                          -> t


(* Sets of types *)
let typemember t  =
let t = typeclass t
in  let rec mem = function 
        | []      -> false
        |        (x::xs) -> t = typeclass x || mem xs
    in mem
    
let typeaugment t ts = if typemember t ts then ts else t::ts

let typediff = fun xs ys ->
    List.fold_right 
    (fun x zs -> if typemember x ys then zs else x::zs) xs [];;
    
let typeunion xs ys = 
    List.fold_right (fun x zs -> if typemember x zs then zs else x::zs) ys xs;;

let typeUnion xss = 
    List.fold_right typeunion xss [];;


(*      
        When we print a type expression, we actually print its class.
*)
        
let rec tstring : typexpr -> string =
fun t -> 
match typeclass t with
|     UnitType        -> "()"
|     NumType         -> "Num"
|     BoolType        -> "Bool"
|     CharType        -> "Char"
|     VarType s       -> s
|     StructType tenv -> 
       "{"^ String.concat "; " (List.map (fun (i, t) -> i ^ ": " ^ tstring t) tenv) ^ "}"
|     PairType (s, t) -> "("^tstring s^","^snd_tstring t^")"
|     WithType (s, t) -> tstring s^" with "^tstring t
|     ListType t      -> "["^tstring t^"]"
|     ConType  (s, ts)-> s^conargs_tstring ts
|     AbbrType (s, ts, ty) -> s^conargs_tstring ts^" = "^ tstring ty
|     FunType  (s, t) -> dom_tstring s^"->"^ran_tstring t
|     ExpandedType (source, expansion) -> tstring source ^ "(≡" ^tstring expansion^"≡)"
|     TypeFunType  (params, body) -> "(Λ(" ^ String.concat "," params ^ ") -> " ^ tstring body ^ ")"
|     PolyType (vs,t) -> "@" ^String.concat "," vs ^"."^tstring t
|     UnknownType u   -> "_"^Pointer.name u

and snd_tstring : typexpr -> string = 
fun t -> 
match typeclass t with
|     PairType (s, t) -> tstring s^","^snd_tstring t
|     other           -> tstring other
    
and ran_tstring : typexpr -> string = 
fun t ->
match typeclass t with
|     FunType (s, t) -> dom_tstring s^"->"^ran_tstring t
|     other          -> tstring other
    
and dom_tstring : typexpr -> string = 
fun t ->
match typeclass t with
|     FunType (s, t) -> "("^dom_tstring s^"->"^tstring t^")"
|     other          -> tstring other
    
and conargs_tstring : typexpr list -> string = fun t -> 
match t with
|  []  -> ""
|  [t] -> 
   (match typeclass t with 
   |  UnitType 
   |  BoolType 
   |  CharType 
   |  VarType _  -> " "^tstring t 
   |  _          -> "("^tstring t^")"
   )
|  ts  -> "("^String.concat ", " (List.map tstring ts)^")"

(*
        There are no infinitary types: so we can't solve a
        constraint such as \verb"_t=(_t->T)" that defines a
        type \emph{directly} in terms of itself. 
        
        The following function
        checks that the unknown which is its first argument
        doesn't appear in the expression which is its second.

*)

let occurs : typexpr -> typexpr -> bool  =
    fun t1 -> 
    let rec occs t =
        let t2 = typeclass t in
        match t2 with
        | UnknownType _     -> sameUnknown t1 t2
        | PairType(tl, tr)  -> occs tl || occs tr
        | WithType(tl, tr)  -> occs tl || occs tr
        | ListType t        -> occs t
        | FunType (tl, tr)  -> occs tl || occs tr
        | ConType (_, ts)   -> List.exists occs ts 
        | ExpandedType(_, exp) -> occs exp
        | _                 -> false
    in
        occs
        

(*
        [equate typeunknown typexpr] sets the given type unknown to
        the given type expression -- provided doing so wouldn't
        generate an infinite type.
        
*)

let equate : typexpr -> typexpr -> unit =
    fun _ty1 _ty2 -> 
       let ty1 = _ty1 and ty2 = _ty2 in
       trace(" -- "^tstring ty1^" := "^tstring ty2);
       match ty2 with 
         UnknownType _ when sameUnknown ty1 ty2  -> ()
       | ty2           when not(occurs ty1 ty2)  -> 
                       setUnknown ty1 ty2
       | ty2  -> failwith ("type "^tstring _ty1^" cannot be the same as "^
                            tstring _ty2)


(*
        We need to know what unknowns appear in a type before we
        generalise it.
*)
open Utils
let rec unknownsInType: typexpr -> typexpr list =
 fun t  ->
 let rec unk t ts = 
     let t = typeclass t in
     match t with
     | PairType(t1, t2)  -> unk t2 (unk t1 ts)
     | WithType(t1, t2)  -> unk t2 (unk t1 ts)
     | FunType(t1, t2)   -> unk t2 (unk t1 ts)
     | ExpandedType(t1, t2) -> unk t2 (unk t1 ts)
     | ListType t        -> unk t ts
     | StructType tenv   -> List.fold_right (fun (i, t) ts -> unk t ts) tenv ts 
     | UnknownType _     -> typeaugment t ts
     | ConType(_, ts')   -> List.fold_right unk ts' ts
     | _                 -> ts
 in  unk t []

let varName: typexpr -> identifier = fun (VarType v) -> v

let mkVarType: identifier -> typexpr = fun v -> VarType v

let rec varNamesInType: typexpr -> identifier list =
 fun t  ->
 let rec unk : typexpr -> typexpr list -> typexpr list = fun t ts -> 
     let t = typeclass t in
     match t with
     | PairType(t1, t2)  -> unk t2 (unk t1 ts)
     | FunType(t1, t2)   -> unk t2 (unk t1 ts)
     | WithType(t1, t2)  -> unk t2 (unk t1 ts)
     | ListType t        -> unk t ts
     | StructType tenv   -> List.fold_right (fun (i, t) ts -> unk t ts) tenv ts 
     | UnknownType _     -> ts
     | ConType(_, ts')   -> List.fold_right unk ts' ts
     | VarType v         -> if List.mem t ts then ts else t::ts
     | ExpandedType(_, t)-> unk t ts
     | _                 -> ts
 in  remdups(List.map varName (unk t []))
 

let mkPolyType typebody =
let typebvs = varNamesInType typebody
in  match typebvs with 
    | []      -> typebody
    | typebvs -> PolyType(typebvs, typebody)




















































