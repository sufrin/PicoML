(*
        Built-in functions of picoML
*)

(*
        This module defines the built-in functions of the \textsf{picoML} library.

*)

open Type
open Semantics

module Local =
struct

    (* \subsubsection*{Wrapper functions}  *)
    
    let strict name f  = 
        Strict 
        (fun x -> 
         try f x with 
         | Exception m  -> except m
         | Failure m    -> failwith m 
         | Sys.Break    -> raise Sys.Break
         | other        -> failwith ("inappropriate operand(s) for strict function: "^name)
        )
        
    let binary name f = 
    strict name 
           (*(fun(NumVal i) -> strict name (fun(NumVal j) -> NumVal(f i j)))*)
           (function  PairVal(l, r) -> 
                      let NumVal l' = force l
                      and NumVal r' = force r in NumVal(f l' r'))

    let unary name f = 
    strict name (fun(NumVal n) -> NumVal(f n))
    
    
    let rel name f g = 
    strict name 
           (function  
            | PairVal(l, r) -> 
                         let l' = force l
                         and r' = force r in begin
                         match (l', r') with
                           | (NumVal i, NumVal j) -> BoolVal(f i j)
                           | _                    -> BoolVal(g l' r')
                         end
            )
                      
    
    (* Structural equality: v, w must be in whnf *)
    let rec deepeq = fun v -> fun w -> let open Number in
        ( match (v, w) with
        |   PairVal(h, t), PairVal(h', t') -> deepeq (force h)(force h') && deepeq (force t)(force t') 
        |   Cons(h, t),    Cons(h', t')    -> deepeq (force h)(force h') && deepeq (force t)(force t')
        |   Nil,           Nil             -> true
        |   NumVal i,      NumVal j        ->  i =/ j
        |   CharVal i,     CharVal j       ->  i = j
        |   BoolVal i,     BoolVal j       ->  i = j
        |   Const i,       Const j         ->  i = j
        |   UnitVal,       UnitVal         ->  true
        |   Inject(i, v'), Inject(j, w')   ->  i = j && deepeq (force v') (force w')
        |   _                              -> false 
        )
    
    (* 
        (possibly lexicographic) ordering v, w must be in whnf 
        some choices here make sense only in the typechecked form
    *)
    let rec deeple = fun v -> fun w -> let open Number in
        ( match (v, w) with
        |   PairVal(h, t), PairVal(h', t') -> deepls (force h) (force h') ||
                                              (deepeq (force h) (force h') && deeple (force t)(force t')) 
        |   Cons(h, t),    Cons(h', t')    -> deepls (force h) (force h') ||
                                              (deepeq (force h) (force h') && deeple (force t)(force t')) 
        |   Nil,           Cons _          -> true
        |   Nil,           Nil             -> true
        |   NumVal i,      NumVal j        ->  Q.leq i j
        |   CharVal i,     CharVal j       ->  i <= j
        |   BoolVal i,     BoolVal j       ->  i <= j
        |   Const i,       Const j         ->  i <= j
        |   Const i,       Inject(j, _)    ->  i <= j (* Arbitrary choice: lexicographic order of constructor name *)
        |   Inject(i, v'), Inject(j, w')   ->  i = j && deeple (force v') (force w')
        |   _                              -> false 
        ) (* TODO: order constructors within types by giving them an ordinal index 
             TODO: implement this more efficiently with a 3-valued compare
          *)
        
    (* structural inequality: v, w must be in whnf *)
    and deepgr = fun v -> fun w -> deepls w v
    and deepls = fun v -> fun w -> deeple v w && not (deepeq v w)
    and deepge = fun v -> fun w -> deeple w v

   (* \subsubsection*{Constructors and selectors} 
   
      Now that we have data declarations and case functions there is
      little need for most of the following to be defined here.
   *) 
                         
    let trueVal  = BoolVal true
    let falseVal = BoolVal false
    
    let some     = fun v -> Inject("Some", v)
    let none     = Const "None"
    let cons     = Strict  
                   (function PairVal(x, xs) -> (incr conscount; Cons(x, xs)))
    let hd       = strict "hd"   (function (Cons(x, _)) ->  x   | Nil -> except "hd Nil")
    let tl       = strict "tl"   (function (Cons(_, xs)) ->  xs | Nil -> except "tl Nil")
    let null     = strict "null" (function Nil-> trueVal | _ -> falseVal)
    let fst      = strict "fst"  (function PairVal(a,  b) -> a)
    let snd      = strict "snd"  (function PairVal(a,  b) -> b)
    let fail     = Strict (fun s -> except (force_string s))
    
    let perform  = Strict Action.performAct
    
(*  \subsubsection*{Character IO} *)
    
    
    let stdIn = strict "stdin"
    (function BoolVal echo ->
         Utils.echo echo;
         listify true (Utf8string.from_channel stdin)
    )
    
    let fileIn = strict "__file"
    (fun path ->
     try 
      let chan = open_in @@ force_string path in
      let gen  = Utf8string.from_channel chan 
      in  some @@ listify false gen                   
     with
      Sys_error s -> none
    )
    
    (* Arguments to a script or program start after the first -F or -f *)
    let argv =
    let rec f = function 
        | []         -> Nil
        | "--"::args -> g args
        | "-f"::args -> g args
        | "-F"::args -> g args
        | _::args    -> f args
        and g = function
        | [] -> Nil
        | arg::args -> Cons(mkString arg, g args)
    in  f (Array.to_list(Sys.argv)) 
    

(* components of a rational *)
   
   let numden = function NumVal n -> 
                let (n, d) = Number.numden_num n in PairVal(NumVal n, NumVal d)
   
   let divmod = function NumVal n -> 
                let (q, r) = Number.divmod_num n in PairVal(NumVal q, NumVal r)
   
(*  \subsubsection*{Number-printing precision} *)

    open Number

    let precision = strict "precision"
    (function NumVal n -> set_print_precision n; UnitVal | _ -> UnitVal)
    
(* \subsubsection*{Convenient notation for a few \textsf{picoML} type expressions} *)

    let (-->) t1 t2  = FunType(t1, t2)
    let num          = NumType
    let boolean      = BoolType
    let list t       = ListType t
    let charstring   = ListType CharType
    let (><) t1 t2   = PairType(t1, t2)
    let a'           = VarType "a"
    let b'           = VarType "b"
    let forall vs ty = PolyType (List.map varName vs, ty)
    let opt t        = ConType("Opt", [t])
    
(*  \subsubsection*{Table of picoML primitive values with their types} *)

let primitives = 
[  
        (* numeric operators *) 

   "+",      binary "+"    ( +/ ),       (num >< num) --> num
;  "-",      binary "-"    ( -/ ),       (num >< num) --> num
;  "*",      binary "*"    ( */ ),       (num >< num) --> num
;  "/" ,     binary "/"    ( // ),       (num >< num) --> num

(* TODO: 
;  "round",  unary "round" round_num,    num --> num
;  "ceil",   unary "ceil"  ceiling_num,  num --> num
*)
;  "int",    unary "int"     integer_num,  num --> num
;  "is_int", strict "is_int" (function NumVal n -> BoolVal (is_int_num n)), num --> boolean
;  "floor",  unary "floor"   floor_num,    num --> num
;  "numden", strict "numden" numden,       num --> (num><num)
;  "divmod", strict "divmod" divmod,       num --> (num><num)
;  "**",     binary "**"     ( **/ ),      (num >< num) --> num (* exponent must be integral *)

        (* numeric (and other) relations *)

;  "=",      rel   "="  ( =/ )   ( deepeq ),  forall [a'] ((a' >< a') --> boolean)
;  "<",      rel   "<"  ( </ )   ( deepls ),  forall [a'] ((a' >< a') --> boolean)
;  ">",      rel   ">"  ( >/ )   ( deepgr ),  forall [a'] ((a' >< a') --> boolean)
;  "<=",     rel   "<=" ( <=/ )  ( deeple ),  forall [a'] ((a' >< a') --> boolean)
;  ">=",     rel   ">=" ( >=/ )  ( deepge ),  forall [a'] ((a' >< a') --> boolean)


        (* lists, pairs, booleans *)

;  "::" ,    cons,         forall [a'] ((a' >< list a') --> list a')
;  "hd" ,    hd,           forall [a'] (list a' -->  a')
;  "tl" ,    tl,           forall [a'] (list a' --> list a')
;  "null",   null,         forall [a'] (list a' --> boolean)
;  "Nil",    Nil,          forall [a'] (list a')
;  "None",   none,         forall [a'] (opt a')
;  "Some",   Lazy some,    forall [a'] (a' --> opt a')
;  "fst",    fst,          forall [a'; b'] ((a'><b') --> a')
;  "snd",    snd,          forall [a'; b'] ((a'><b') --> b')
;  "True",   trueVal,      boolean
;  "False",  falseVal,     boolean
;  "unicode",    strict "unicode"   (function CharVal c -> NumVal(Q.of_int @@ Uchar.to_int c)), CharType --> num
;  "unidecode",  strict "unidecode" (function NumVal  i -> CharVal(Uchar.of_int @@ Z.to_int @@ Q.num i)), num --> CharType
;  "stdin",  Opaque("InputHandle",  `InChan stdin),     ConType("InputHandle", [])
;  "stdout", Opaque("OutputHandle", `OutChan stdout),   ConType("OutputHandle", [])
;  "stderr", Opaque("OutputHandle", `OutChan stderr),   ConType("OutputHandle", [])
;  "num2string", strict "num2string" (function NumVal i -> mkString @@ string_of_num i), num --> charstring
  
(* miscellaneous pseudo-functions *)

;  "__stdin",  stdIn,        (boolean     -->    charstring)
;  "__file",   fileIn,       (charstring  -->    opt charstring)
;  "argv",     argv,         list(list(CharType))
;  "eof",      eof,          CharType
;  "prec",     precision,    num --> UnitType
;  "fail",     fail,         forall [a'] (list CharType --> a')
;  "force",    Strict force, forall [a'] ( a' --> a')

(* "unsafely" perform an action at the moment of reduction *)
;  "__unsafePerformAction",  perform, forall [a'] (ConType("ACT", [a']) --> a')
]

let types =
[ "Num",          NumType
; "Unit",         UnitType 
; "Bool",         BoolType 
; "Char",         CharType 
; "Opt",          opt a'
; "String",       ExpandedType(ConType("String", []), ListType CharType)
; "OutputHandle", ConType("OutputHandle", [])
; "InputHandle",  ConType("InputHandle", [])
]

end

let valueEnv = ref(Mapping.fromList (fun (name, value, typ) -> (name, value)) Local.primitives)
let typeEnv  = ref(Mapping.fromList (fun (name, value, typ) -> (name, typ))   Local.primitives)
let typeDefs = ref(Mapping.fromList (fun (name, typ) -> (name, typ)) Local.types)
let trueVal  = Local.trueVal 
let falseVal = Local.falseVal
let some     = Local.some
let none     = Local.none
let eof      = Semantics.eof


(* *********************************************************** *)






















