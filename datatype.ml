(*
        Datatype implementation
        
        $Id: datatype.ml 175 2019-08-15 22:32:51Z sufrin $
*)

open Type
open Syntax
open Semantics
open Mapping

let print_defs : typexpr map -> unit =
fun defs -> 
    Mapping.iterpairs 
       (fun i t -> if not (Syntax.holeId i) then Format.printf "%s ≘ %s@." (istring i) (tstring t)) 
       defs

(*
        This generates the constructor, predicate, and projector corresponding
        to the data constructor with a given name.
*)

let constructorMeaning name =
(       Lazy   (fun v -> Inject(name, v))
,       Strict (function 
               | Inject(name', _) -> BoolVal(name=name') 
               | _                -> Builtin.falseVal
               )
,       Strict (function 
               | Inject(name', v) when name'=name -> v 
               | Inject(name', v) -> failwith ("Wrong constructor ("^name'^") for pr_"^name)
               | _                -> failwith ("Badly-typed operand for pr_"^name)
               )
)

(*
        This generates the constructor, and predicate corresponding
        to the data constant with a given name.
*)

let constantMeaning  name = 
    let value = Const name
    in  (value, Strict (fun value' -> BoolVal(value==value')))


(* 

   Here we check the consistency of a new collection of data type
   declarations, and yield the extensions to the value and type
   environments that they yield.

   The scope of a data type being declared is the whole collection
   of types being declared simultaneously, including itself.

   Simple example: data RoseTree a = Rose (a, [RoseTree (a)])

   The easiest way to do this is in two passes: in the first we
   bind the (new) type names as if their definitions were well-formed.
   In the second we check that the make the semantic bindings and
   nnormalize the various types, during which we check that all
   types were declared.
   
*)

type context = (typexpr map * typexpr map * value map)

let declareData : typexpr map -> typexpr map -> datadecl list -> context = 
fun globaldefs env decls ->
    let declareType: context -> datadecl -> context =
    fun (defs, env, venv) (tycon, ps, alts) -> 
    let ty  = ConType(tycon, List.map mkVarType ps) in
    let def = ty (* TypeFunType(ps, ty) *)
        in
        let declareAlt: context -> alt -> context = 
        fun (defs, env, venv) -> function
        | (datacon, None) ->
          let v, is = constantMeaning datacon in
             (defs
             ,env  |++| (datacon |-> mkPolyType ty) 
                   (* |++| ("is"^datacon |-> mkPolyType  (FunType(ty, BoolType))) *)
             ,venv |+|  (datacon|->v) 
                   (* |+|  (("is"^datacon)|->is) *)
             )
        | (datacon, Some domty)   -> 
          let v, is, pr = constructorMeaning datacon in
          let domty = Infer.normalizeType (globaldefs |+| defs) domty in
          let datacon' = Utils.stripslosh datacon in
             (defs
             ,env  |++| (datacon |-> mkPolyType   (FunType(domty, ty))) 
                   (* |++| ("is"^datacon' |-> mkPolyType  (FunType(ty, BoolType)))
                      |++| ("inv"^datacon' |-> mkPolyType  (FunType(ty, domty)))
                   *)
             ,venv |++| (datacon|->v) 
                   (*
                   |+|  (("is"^datacon')|->is)
                   |+|  (("inv"^datacon')|->pr)
                   *)
             )
        in 
        let (defs, env, venv) = List.fold_left declareAlt (defs, env, venv) alts
        in (defs (* No need for |+| (tycon |-> def) because it's already present in defs *), env, venv)
    in  let preDeclareType: typexpr map -> datadecl -> typexpr map = 
            fun defs (tycon, ps, _) -> 
                      let def = ConType(tycon, List.map mkVarType ps) in defs |+| (tycon |-> def)
    in  let newdefs = 
            List.fold_left preDeclareType emptymap decls
    in  
        List.fold_left declareType (newdefs, emptymap, emptymap) decls

let declareType : typexpr map -> typedecl list -> typexpr map = 
fun globaldefs decls ->
    let declareAbbr: typexpr map -> typedecl -> typexpr map =
    fun defs (tycon, ps, ty) -> 
    let ty  = Infer.normalizeType (globaldefs |+| defs) ty in
    let def = match ps with 
              |   [] -> ExpandedType(ConType(tycon, []), ty)
              |   _  -> TypeFunType(ps, ty)
    in  defs |+| (tycon |-> def)
    in  List.fold_left declareAbbr emptymap decls














