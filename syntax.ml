(*
        This module defines the abstract syntax of picoML expressions
        and declarations, together with simple functions for 
        pretty-printing them. 
*)
open Type
open Printf

type identifier = string
type location = int * int * string
let  nowhere: location = (0, 0, "") 

(* We use a little heuristic to avoid duplication of a long path in the output 
   of nested terms.
*)

let  lastPath = ref "" 
let  clearPath() = lastPath := ""

let lstring: location -> string = 
fun (line, char, path) -> 
    let epath = if false && path = !lastPath then "" else (lastPath:=path; path)
    in  sprintf "%s:%d.%d" epath line char


type    expr    =      
|       Unit
|       Id      of identifier
|       ConId   of identifier
|       Hole    of identifier
|       Num     of Number.num
|       String  of string
|       Char    of Utf8string.unicodechar
|       Pair    of expr*expr
|       With    of expr*expr
|       Apply   of expr*expr
|       Select  of expr*identifier
|       Inside  of expr*expr
|       Fun     of identifier*expr
|       Let     of decl * expr
|       Struct  of decl
|       If      of expr*expr*expr
|       CaseFun of (expr (*pat*) * expr) list
|       At      of expr * location
|       Has     of expr * typexpr

and     decl    =       
|       ValDec   of expr (* pattern *) * expr
|       AndDec   of decl*decl
|       SeqDec   of decl*decl
|       WhereDec of decl*decl
|       RecDec   of decl
|       HasType  of identifier*typexpr*location


(* Hole generation *)
let holeNum = ref 0

let mkHole() = begin
    incr holeNum;
    Hole(Format.sprintf "_#%d" !holeNum)
end

let holeId s = s.[0]='_' && s.[1]='#'


(* Remove location information from patterns *)    
let rec stripAt: expr -> expr = fun e -> match e with
| At(e, _)     -> stripAt e
| Apply(e, f)  -> Apply(stripAt e, stripAt f)
| Select(e, f) -> Select(stripAt e, f)
| Pair(e, f)   -> Pair(stripAt e, stripAt f)
| other        -> other

let rec locate = function
| At(_, loc)   -> loc
| Apply(e, f)  -> locate e
| Inside(e, f) -> locate e
| Select(e, f) -> locate e
| Pair(e, f)   -> locate e 
| _            -> (-1, -1, "unknown position") 


(* fixity determination: linked dynamically in Top *)

let _isOp: (string -> bool) ref = ref @@ fun (x:string) -> false

let isOp str = ! _isOp str

let setIsOp f = _isOp := f

(* ************************* *)

let istring s = if isOp s then  "("^s^")" else s

let rec estring : expr -> string = fun e -> estringw true e

and estringn: expr -> string = fun e -> estringw false e

and estringw top = function
    |   Unit          -> "()"
    |   Hole s        -> istring s
    |   Id  s         -> istring s
    |   ConId  s      -> istring s
    |   Num i         -> Number.string_of_num i
    |   String i      -> "\""^i^"\""
    |   Char   i      -> sprintf "'%s'" (Utf8string.to_utf8string i)
    |   Pair(e1, e2)  -> sprintf "(%s,%s)" (estringn e1) (estringn e2)
    |   With(e1, e2)  -> sprintf "%s with %s" (estring e1) (estring e2)
    |   Apply(Id e1, Pair(e0, e2)) when isOp e1 
                      -> sprintf "(%s %s %s)" (estringn e0) e1 (estringn e2)
    |   Apply(ConId e1, Pair(e0, e2)) when isOp e1 
                      -> sprintf "(%s %s %s)" (estringn e0) e1 (estringn e2)
    |   Apply(e1, e2) -> sprintf "(%s %s)" (estringn e1) (estringn e2)
    |   Inside(e1, e2) -> sprintf "(%s /// %s)" (estringn e1) (estringn e2)
    |   Select(e1, f) -> sprintf "(%s.%s)" (estringn e1) (istring f)
    |   Fun(i, e)     -> sprintf "λ {%s -> %s}"  i (estringn e)
    |   CaseFun cases -> "⟨" ^ (String.concat " | " (List.map cstring cases)) ^ "⟩"
    |   Let(d, e)     -> sprintf "let %s in %s end"  (dstring d) (estringn e)
    |   Struct(d)     -> sprintf "{%s}"  (dstring d) 
    |   If(g, e1, e2) -> sprintf "if %s then %s else %s" (estringn g) (estringn e1) (estringn e2)
    |   Has(e, t)     -> sprintf "(%s : %s)" (estring e) (tstring t)
    |   At(e, loc)    -> if top then 
                            sprintf "%s (at %s)" (estring e) (lstring loc)
                         else
                            estringn e

and cstring : (expr * expr) -> string = 
    fun (p, e) -> sprintf "%s -> %s"  (estringn p) (estringn e)

and sndestring : expr -> string = function
    |   Pair(e1, e2) -> sprintf "%s,%s" (estring e1) (sndestring e2)
    |   other        -> estring other

and dstring : decl -> string = function
    |   ValDec(pat, e)   -> sprintf "%s = %s" (estring pat) (estring e)
    |   AndDec(d1, d2)   -> sprintf "%s and %s" (dstring d1) (dstring d2)
    |   SeqDec(d1, d2)   -> sprintf "%s ; %s" (dstring d1) (dstring d2)
    |   WhereDec(d1, d2) -> sprintf "%s where %s" (dstring d1) (dstring d2)
    |   RecDec d         -> sprintf "rec %s" (dstring d)
    |   HasType(i, t, _) -> sprintf "%s: %s" (istring i) (tstring t)
    

type    alt    = string * typexpr option

type datadecl = string * string list * alt list

type typedecl = string * string list * typexpr   (* Constructor (params) = type *)

type    phrase = 
|       Expr   of expr 
|       Decl   of decl 
|       Data   of datadecl list
|       Type   of typedecl list
|       Error  of string
|       Eof
|       Located of phrase*location


let rec declared: decl -> identifier list = function
    | AndDec(d1, d2)   -> declared d1 @ declared d2
    | SeqDec(d1, d2)   -> declared d1 @ declared d2
    | WhereDec(d1, d2) -> declared d1
    | RecDec d         -> declared d
    | ValDec (pat, _)  -> declaredInPat pat
    | HasType(i, _, _) -> []    
    
and declaredInPat: expr -> identifier list = fun pat -> match pat with
    |  At(e, _)              -> declaredInPat e
    |  Id i                  -> [i]
    |  Hole i                -> [i]
    |  Pair (p1, p2)         -> declaredInPat p1 @  declaredInPat p2
    |  Apply (ConId id, pat) -> declaredInPat pat
    |  Has (pat, _)          -> declaredInPat pat
    |  _                     -> []

let rec holeIn: expr -> bool = function 
|       Unit    -> false
|       Id      _ -> false
|       ConId   _ -> false
|       Hole    _ -> true
|       Num     _ -> false
|       String  _ -> false
|       Char    _ -> false
|       Select  _ -> false
|       Struct  _ -> false
|       Inside _  -> false
|       With   _  -> false
|       Pair    (e1, e2) -> holeIn e1 || holeIn e2
|       Apply   (e1, e2) -> holeIn e1 || holeIn e2
|       Fun     (_, e) -> holeIn e
|       Let     (d, e) -> holeInDec d || holeIn e
|       If      (e1, e2, e3) -> holeIn e1 || holeIn e2 || holeIn e3
|       CaseFun cases -> List.exists (fun (pat, rhs) -> holeIn rhs) cases
|       At      (e, _) -> holeIn e 
|       Has     (e, _) -> holeIn e

and     holeInDec = function       
|       ValDec   (pat, def) -> holeIn def
|       AndDec   (d1, d2)   -> holeInDec d1 && holeInDec d2 
|       SeqDec   (d1, d2)   -> holeInDec d1 && holeInDec d2
|       WhereDec (d1, d2)   -> holeInDec d1 && holeInDec d2
|       RecDec   d -> holeInDec d
|       HasType  _ -> false

let holeInPhrase = function
|       Expr   e -> holeIn e
|       Decl   d -> holeInDec d
|       _        -> false















