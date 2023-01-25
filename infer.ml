
(*
        This module defines the main components of the
        type inference engine. 
        
        The following modules are used.
*)

open Syntax
open Type
open Utils
open Pointer
open Format
open Env
open Mapping
open Trace

let traceBefore expr = 
    let _ = inc() in
    trace("|- "^estring expr^" ?")

let traceAfter expr r = 
    trace ("|- "^estring expr^" : "^tstring r);
    dec()
    
exception TypeError of location option * string * env
let typeErrorAt: location->string->env->'a = fun loc message env -> raise (TypeError (Some loc, message, env))
let typeError message env = raise (TypeError (None, message, env))

let rec flatten: typexpr -> typexpr list = function 
| PairType(l, r) -> l :: flatten r
| other -> [other]

let rec normalizeType: env -> typexpr -> typexpr = 
fun defs ty -> match ty with
|   UnitType
|   NumType
|   BoolType
|   CharType        -> ty
|   FunType  (l, r) -> FunType(normalizeType defs l, normalizeType defs r) 
|   PairType (l, r) -> PairType(normalizeType defs l, normalizeType defs r)
|   WithType (l, r) -> 
    begin match (expandedType @@ normalizeType defs l, expandedType @@ normalizeType defs r) with
          | StructType lmap, StructType rmap -> StructType (lmap |+++| rmap)
          | _l, _r -> failwith (Format.sprintf "%s with %s (only record types can be joined by 'with')" (tstring l) (tstring r))
    end
|   ListType t      -> ListType(normalizeType defs t)
|   ConType  (tid, params) -> 
    let params = List.map (normalizeType defs) params in 
        (match defs |?| tid with
        | TypeFunType(args, body) ->
                  if List.length args==List.length params then
                     let defs = List.map2 (fun n v -> (n, v)) args params 
                     in  ExpandedType(ty, Env.substitute defs body) (* Show provenance *)
                  else 
                  (* A tuple of types is built from right-associated pairs, so there's an
                     ambiguity here that's only fixable by (a) having variadic tuple
                     constructors, or (b) persuading the programmer to name the 
                     last type of a tuple of types if it is itself a tuple. The former
                     will happen eventually. 
                   *)
                  if List.length args>1 && List.length params==1 then
                     (* Maybe a tuple of parameters *)
                     match List.hd params with
                     | PairType _ -> 
                     let defs = List.map2 (fun n v -> (n, v)) args (flatten @@ List.hd params) 
                     in  Env.substitute defs body                    
                     | _ -> failwith ("type abbreviation "^tid^" (wrong number of parameters) "^tstring ty)
                  else
                     failwith ("type abbreviation "^tid^" (wrong number of parameters) "^tstring ty)
        |  ConType _ as ty' -> Trace.trace(Format.sprintf "%s ==> %s" (tstring ty) (tstring ty'));
                       ty (* made during the first pass for a DATA declaration *)   
        | other     -> ExpandedType(ty, other)
        )
|   StructType   map -> StructType (Mapping.map (normalizeType defs) map)
|   PolyType     (vs, t) -> PolyType(vs, normalizeType defs t)
|   ExpandedType (_, t)  -> normalizeType defs t
|   UnknownType  _ (**** TODO ****)
|   VarType      _
|   TypeFunType  _ 
|   AbbrType     _ -> ty

and expandedType = function
    | (ExpandedType (_, t)) -> t 
    | other -> other 

   


(* \subsection*{Generalisation of a type} *)

(*
        $abstract~unknowns~t$ replaces each occurence of one of
        the unknowns in $t$ by a type variable (whose name, as
        it happens, is derived from the name of the unknown).
        The choice of name for the type variable is arbitrary,
        but convenient for our purposes, since an unknown which
        prints (while tracing) as $\_x$ will be abstracted as $x$.
*)

let rec abstract : typexpr list -> typexpr -> typexpr =
    fun unknowns t ->
    let t = typeclass t in
    match t with
      UnknownType u when List.mem t unknowns -> VarType (Pointer.name u)
    | PairType(t1, t2)  -> PairType (abstract unknowns t1, abstract unknowns t2)
    | FunType (t1, t2)  -> FunType  (abstract unknowns t1, abstract unknowns t2)
    | ExpandedType (t1, t2) -> ExpandedType  (abstract unknowns t1, abstract unknowns t2)
    | ListType t        -> ListType (abstract unknowns t)
    | ConType(name, ts) -> ConType(name,  List.map (abstract unknowns) ts)
    | StructType map    -> StructType (Mapping.map (fun t -> abstract unknowns t) map)     
    | _ -> t
    

(*
        $$generaliseWrt ~env ~t = ~t'$$ implements  $$env |- t<<t'$$
        
\begin{enumerate}
        \item Make a list of the unknowns which appear in the environment --
              these are the ones which we must not abstract. 
              
              This step happens only once during the computation of 
              $Mapping.map ~(generaliseWrt ~env) ~env'$
           
        \item Make a list of the unknowns in $t$, and remove from it the
           unknowns from the environment; the remaining unknowns
           are those which must be abstracted from $t$.
        
\end{enumerate}     

                
*)
let generaliseWrt env =
let inEnv = unknownsInEnv env 
in
let generalise t =
    let tt       = typeclass t in
    let unknowns = unknownsInType tt -- inEnv in
    match unknowns with
    |  [] -> tt
    |  _  -> PolyType(List.map unknownName unknowns, abstract unknowns tt)
in generalise 
 
(*i *************************************************************************** i*)

let rec typeassertions: bool -> decl -> env =
    fun topLevel d ->
    match d with
    | AndDec(d1, d2)    -> typeassertions topLevel d1 @ typeassertions topLevel d2
    | SeqDec(d1, d2)    -> typeassertions topLevel d1 @ typeassertions topLevel d2
    | WhereDec(d1, d2)  -> typeassertions topLevel d1 
    | RecDec d          -> typeassertions topLevel d
    | ValDec (i, _)     -> emptymap
    | HasType(i, t, loc)  -> if not topLevel && isPoly t then
                              typeErrorAt loc (Format.sprintf "nested hinted type is polymorphic: %s: %s" i (tstring t)) emptymap
                           else
                              i|->t      
                               
let rec checktypeassertions: bool -> decl -> bool=
    fun topLevel d ->
    match d with
    | AndDec(d1, d2)    -> checktypeassertions topLevel d1 && checktypeassertions topLevel d2
    | SeqDec(d1, d2)    -> checktypeassertions topLevel d1 && checktypeassertions topLevel d2
    | WhereDec(d1, d2)  -> checktypeassertions topLevel d1 && checktypeassertions topLevel d2
    | RecDec d          -> checktypeassertions topLevel d
    | ValDec (i, _)     -> true
    | HasType(i, t, loc)  -> if not topLevel && isPoly t then
                              typeErrorAt loc (Format.sprintf "nested hinted type is polymorphic: %s: %s" i (tstring t)) emptymap
                           else
                              true       

type locationmap = location map

let rec typeassertionlocations: decl -> locationmap  =
    fun d ->
    match d with
    | AndDec(d1, d2)    -> typeassertionlocations d1 @ typeassertionlocations d2
    | SeqDec(d1, d2)    -> typeassertionlocations d1 @ typeassertionlocations d2
    | WhereDec(d1, d2)  -> typeassertionlocations d1
    | RecDec d          -> typeassertionlocations d
    | ValDec (i, _)     -> emptymap
    | HasType(i, t, l)  -> i|->l     
    
(*
        Given a pair of type expressions, this procedure
        succeeeds if it can make the two expressions (look)
        identical (by means of zero or more assignments to
        unset type unknowns contained in the expressions).

        If it succeeds then any necessary type assignments have
        been made. If it fails then some type assignments may
        have been made. These can't be retracted, but in the
        context in which this particular unification algorithm
        is being used, that doesn't matter.

*)
        
let rec unify: typexpr -> typexpr -> unit =
    fun _ty1 _ty2 ->
    let ty1 = typeclass @@ expand _ty1 in
    let ty2 = typeclass @@ expand _ty2
    in
    Trace.trace(Format.sprintf " -- %s =::= %s" (tstring @@ expand ty1) (tstring @@ expand ty2));
    match expand ty1, expand ty2 with
    |   UnitType,          UnitType            -> ()
    |   NumType,           NumType             -> ()
    |   BoolType,          BoolType            -> ()
    |   CharType,          CharType            -> ()
    |   ListType t,        ListType t'         -> unify t t'
    |   PairType(t1, t2),  PairType(t1', t2')  -> unify t1 t1'; unify t2 t2'
    |   StructType tenv,   StructType tenv'    -> unifyenv tenv tenv'
    |   FunType (t1, t2),  FunType (t1', t2')  -> unify t1 t1'; unify t2 t2'
    |   ConType (s1, ts1), ConType (s2, ts2) when s1=s2 -> List.iter2 unify ts1 ts2
    |   UnknownType _,    _                    -> equate (expand ty1) (expand ty2)
    |   _,                UnknownType _        -> equate (expand ty2) (expand ty1)
    |   _  -> failwith ("types "^tstring ty1^" and "^
                         tstring ty2^" do not match.")

(*      \subsection*{Unification}
        Unify a pair of environments with each other
*)
and unifyenv : env -> env -> unit =
    fun env1 env2 ->
    Trace.trace("-- Unifying "^envstring env1^" with "^envstring env2);
    let check i t1 = 
        try unify t1 (env2 |?| i) with
            Failure message ->
            typeError (i ^ " cannot be consistently typed, because\n"^message) env1
    in  iterpairs check env1




let substructure : env -> env -> unit =
    fun env1 env2 ->
    Trace.trace("-- Test "^envstring env1^" is a subtructire of "^envstring env2);
    let check i t1 = 
        try unify t1 (env1 |?| i) with
            | Failure message ->
              typeError (i ^ " cannot be consistently typed, because\n"^message) env1
            | Unbound _ -> failwith ("Field "^i^" is missing.")
    in  iterpairs check env2

let assumptions env =
    if false then
    "\nCurrent type assumptions are:\n"^  envstring env
    else "" 

let rec etype: env -> env -> expr -> typexpr =
fun defs env e -> 
let normalType expr = typeclass (normalizeType defs @@ etype defs env expr) in
let result = 
    traceBefore e;
    match e with
    |   At(e', loc)   -> Trace.trace(Format.sprintf "||== %s at %s" (estring e') (lstring loc)  );
                         (try etype defs env e' with 
                         |  TypeError(None, s, env) -> typeErrorAt loc s env
                         |  Failure s   -> typeError (s^" "^estring e) env
                         |  Unbound s   -> typeError ("Unbound "^s^" in "^estring e) env                         
                         )
    |   Unit          -> UnitType
    |   Num _         -> NumType
    |   Hole  i       -> idtype env i
    |   Id    i       -> idtype env i
    |   ConId i       -> idtype env i
    |   String  _     -> ListType CharType
    |   Char    _     -> CharType
    |   Has(e, t)     ->
           let te = expand @@ etype defs env e in
           let ti = expand @@ instantiate @@ normalizeType defs t in begin
               Trace.trace(Format.sprintf "%s ::: %s" (tstring te) (tstring ti));
               try
               (match (te, ti) with
               |   StructType tenv, StructType tenv' -> substructure tenv tenv'; ti
               | _                                   -> unify te ti; ti
               ) 
               with
                 Failure message -> 
                   typeError ("Error in type hint: "^message^"\n"^estring e^": "^tstring te^" should have type consistent with "^tstring ti) env
           end       
    |   If (g, e1, e2)-> 
        let tg  = etype defs env g in
        let t1, t2  = etype defs env e1, etype defs env e2 in
            trace("-- "^tstring t1 ^ " == " ^ tstring t2);
            (try unify t1 t2 with
             Failure message  -> 
                 typeError (estring e1^": "^tstring t1^" and \n"^
                            estring e2^": "^tstring t2^
                            " should have consistent types in a conditional.") env
            );
            trace("-- "^tstring tg ^ " == bool");
            (try unify tg BoolType with
               Failure message  -> 
                 typeError (estring g^" should be a Bool in a conditional but has type: "^
                            tstring tg) env
            );
            t1
    |   Pair (e1, e2) -> PairType(etype defs env e1, etype defs env e2)
    |   With (e1, e2) -> begin match (normalType e1, normalType e2) with
                               | StructType m1, StructType m2 -> StructType (m1 |+++| m2)
                               | l, r -> typeError (Format.sprintf "%s %s with %s (only records can be joined)" (estring e) (tstring l) (tstring r) ) env
                         end
    |   Select (str, f) ->
        (match typeclass(normalizeType defs @@ etype defs env str) with
         | StructType env' -> idtype env' f
         | other           -> 
            typeError (estring str^": "^tstring other^" should be a structure in " ^ estring e) env
        )
    |   Inside (str, body) ->
        (match typeclass(normalizeType defs @@ etype defs env str) with
         | StructType env' -> etype defs (env |+| env') body
         | other           -> 
            typeError (estring str^": "^tstring other^" should be a structure in " ^ estring e) env
        )
    |   Apply(e1, e2) -> 
        let tr = newUnknown() in
         trace("-- Assume "^estring e ^ " : " ^ tstring tr);
         let t1 = etype defs env e1 in
         let t2 = etype defs env e2 in
         let tf = FunType(t2, tr)
         in  trace("-- "^tstring t1 ^ " == " ^ tstring tf);
             (try unify t1 tf with  
                 Failure message -> 
                   typeError (estring e1^ ": "^ tstring t1^
                             "\ncan't be applied to\n"^
                             estring e2^ ": "^ tstring t2 ^ "\nbecause "
                             ^message) env
             );
             tr
    |   Fun(bv, e) ->
        let tbv  = newUnknown()             in
        let env' = env |+| (bv |-> tbv)     in
        let te   = etype defs env' e        in FunType(tbv, te)
    |   CaseFun cases ->
        (* Compute the (pat, body) type pairs for each case
           Unify all the pattern types with the first pattern type, tArg. 
           Unify all the body types with the first body type, tRes.
           The type of the function is tArg -> tRes.
        *)
        let caseType: (expr*expr) -> (typexpr*typexpr) = 
            fun (pat, body) ->
                let genEnv   = substitution(declaredInPat pat) in
                let patType  = etype defs (env |+| genEnv) pat in
                let bodyType = etype defs (env |+| genEnv) body in
                trace(Format.sprintf "-- CASE %s: %s -> %s: %s" (estring pat) (tstring patType) (estring body) (tstring bodyType));
                (patType, bodyType)
        in
        let (tArg, tRes) :: cases = List.map caseType cases 
        in 
           List.iter (fun (tpat, tbody) -> unify tArg tpat; unify tRes tbody) cases;
           FunType (tArg, tRes)  
    |   Struct d ->       
        let env'  = typeEnv false emptyenv defs env d in 
            trace("struct ==> "^envstring env');
            StructType env'       
    |   Let(d, e) ->
        let env'  = declTypes false defs env d         
        in  trace("==> "^envstring env');
            etype defs (env |+| env') e

in let _ = traceAfter e result in
   normalizeType defs @@ result
   
and idtype : env -> identifier -> typexpr =
fun env i ->
    let t = (env |?| i) in 
    match t with
    | PolyType _ -> trace("-- Instantiate "^tstring t); instantiate t
    | _          -> instantiate t
   
    
and declTypes : bool -> env -> env -> decl -> env =
fun topLevel defs env d -> 
    checktypeassertions topLevel d; 
    let assertions = typeassertions topLevel d in
    let env'       = genTypeEnv topLevel assertions defs env d in
    let map        = typeassertionlocations d
    in  
        checkConsistent defs env env' assertions map; 
        simplify (env' |+| assertions)
     

(* 

   The  sequencing enforced below by the two nested lets is crucial
   to soundness of the generalisation algorithm.  The typeEnv
   calculation can unify unknowns in env, but that all has to be
   done before the list of unknowns in env is
   constructed.\footnote{\noindent OCaml compilers don't make any
   guarantees about order of evaluation of the parameters of a
   function -- indeed the bytecode and opt compilers do things in
   exactly the opposite order. For this reason it would be unsafe
   to try to do the obvious substitution optimisation on the
   declarations in $genTypeEnv$.}
   
*)

and genTypeEnv : bool -> env -> env -> env -> decl -> env = fun topLevel assertions defs env d -> 
let env'       = typeEnv topLevel assertions defs env d in
let generalize = generaliseWrt env  in  
    Mapping.map standardise (Mapping.map generalize env') 


and typeEnv : bool -> env -> env -> env -> decl -> env =
fun topLevel assertions defs env d ->
    match d with
    |   ValDec(pat, e)   -> patEnv topLevel assertions defs env pat e
    |   AndDec(d1, d2)   -> typeEnv topLevel assertions defs env d1 |++| 
                            typeEnv topLevel assertions defs env d2 
    |   SeqDec(d1, d2)   -> let env' = genTypeEnv  topLevel assertions defs env d1 
                            in  env' |++| genTypeEnv topLevel assertions defs (env|+|env') d2 
    |   WhereDec(d1, d2) -> let env' = declTypes topLevel defs env d2 
                            in 
                            (* Format.printf "(typing %s) env=%s env'=%s" (dstring d) (envstring env) (envstring env'); *)
                            let res = declTypes topLevel  defs (env|+|env') d1 
                            in  Format.printf "(done typing %s) env'=%s res=%s" (dstring d) (envstring env') (envstring res);
                                res
    |   RecDec d  -> 
        let env'  = substitution (declared d) in 
        trace("-- Assume "^envstring env');
        let env'' = declTypes topLevel defs (env |+| env') d in
            unifyenv env' env'';
            env''
    |   HasType _  -> []
        
and patEnv: bool -> env -> env -> env -> expr (* pat *) -> expr (* rhs *) -> env =
fun topLevel assertions defs env pat rhs -> 
let patVars = declaredInPat pat
in
let genEnv = substitution patVars
in  trace ("-- Typing pattern "^ estring pat^" in patEnv: "^envstring genEnv);
    let patType = etype defs (env |+| genEnv) pat
    in  trace ("-- Assume "^ estring pat^": "^tstring patType^" // in patEnv: "^envstring genEnv);
    let trhs = 
    (* 
         Process any relevant type assertions for functions. This
         is necessary because the standard inference algorithm
         cannote USE the type of bound variables of a function
         expression when checking its body, and (for simplicity)
         the inference algorithm REQUIRES the expression in a
         field-selection expression expr.field to have a record
         type. 
    *)
    (match patVars with
    |   [id] -> (try
                   let asserted = normalizeType defs @@ (assertions |?| id) in
                   let assertedinstance = instantiate asserted  in
                       trace (Format.sprintf "-- Asserted type of %s: %s (declared as %s) // in patEnv " id (tstring assertedinstance) (tstring asserted));
                       match assertedinstance, rhs with
                       |   FunType _, CaseFun _ -> etype defs env (distributeHas(rhs, assertedinstance))
                       |   FunType _, Fun _     -> etype defs env (distributeHas(rhs, assertedinstance))
                       |   _                    -> (etype defs env rhs)

                 with
                 |   Unbound _ -> trace ("-- No user declaration for "^id); (etype defs env rhs)
                )
    |   _    -> (etype defs env rhs)
    )
    in
    (try  unify patType trhs with 
    | Failure message -> 
        typeError (Format.sprintf "pattern %s cannot be matched with expression %s\nbecause %s" (estring pat) (estring rhs) message)
                  env
    );
    genEnv
    
and distributeHas (e, ty) = match (e, ty) with (* Annotate with type assertions *)
|   Fun(bv, body), FunType (targ, tres) -> CaseFun[(distributeHas(Id bv, targ), distributeHas(body, tres))]
|   CaseFun cases, FunType (targ, tres) -> CaseFun(List.map (fun(pat, body)->(distributeHas (pat, targ), distributeHas(body, tres))) cases)
|   _                                   -> Has(e, ty)
    
(*  \newpage
    The function [checkConsistent] checks that two environments are consistent.
    It is assumed that the first environment contains
    standardised types, and that the second is constructed
    entirely from the user's type assertions.

    The workhorse is [checkDecl]. It first checks by unification
    that (a fresh instance of) the declared type is unifiable
    with (a fresh instance of) the inferred type.

\begin{itemize}
    \item If so, then there still might be a problem, for consider:
        
        \begin{quote}
        \begin{tabular}{ll} 
                declared map: &[@x,y. (int->y) -> [x] -> [y]]\\
                inferred map: &[@x,y. (x  ->y) -> [x] -> [y]]
        \end{tabular}
        \end{quote}    

        Here the declared type is  wrong. For example it would admit
        the following application (with $x$ as $string$ and $y$ as $int$)) 
        \begin{center}        
        {[map (\ n . n+1) ["foo", "baz"]]}
        \end{center}        
        which would certainly lead to a nasty surprise at runtime!
        
        But the fresh instances \emph{are} unifiable, giving (for some unknown [_b])
        \begin{center}               
        {[(int->_b)->([int]->[_b])]}
        \end{center}        
        it is the fact that this generalises to 
        \begin{center}               
        {[@a. (int->a)->([int]->[a])]}
        \end{center}        
        that lets us see that something is wrong, for this has fewer
        type variables than the declared type. 
        
        On the other hand there's nothing wrong with declaring
        \begin{center}                
        {[map: @y. (int->y) -> [int] -> [y]]}
        \end{center}        
        for generalising the unified fresh instances yields
        \begin{center}                
        {[@a. (int->a)->([int]->[a])]}
        \end{center}        
        which is just an alpha conversion of the declared type. 

    \item If not, then the declared type is just wrong.

\end{itemize}
*)

(*
\newpage
*)

and checkConsistent : env -> env -> env -> env -> locationmap -> unit =
fun defs contextenv inferenv userenv map ->
let errors = ref [] 
in
let error id uty infty guess =
    errors := (id, uty, infty, guess) :: !errors
    
in  let checkDecl id _userty =
    let userty = expand  _userty in
    let inferty =
        try inferenv |?| id with
            Unbound _ ->
              (* If there isn't a definition the type assertion is a mistake *)
              typeError 
              (Format.sprintf "No implementation for %s: %s  as specified at %s" 
                              id (tstring userty) (lstring (map |?| id))) inferenv
    in
    let userinstance  = instantiate  @@ normalizeType defs @@ userty
    and inferinstance = instantiate  @@ normalizeType defs @@ inferty
    in
    trace @@ Format.sprintf "Initial consistency check %s: \n declared as %s\n inferred as %s" id (tstring userinstance)(tstring inferinstance);
    try 
      unify userinstance inferinstance;
      trace @@ Format.sprintf "(after unification)\n %s: declared %s\n %s: inferred %s\n" id (tstring userinstance) id (tstring inferinstance);
      let genty     = standardise (generaliseWrt contextenv userinstance)
      and declty    = standardise @@ normalizeType defs @@ userty
      in
      
      
      trace @@ Format.sprintf "(generic)\n %s: generalized %s\n %s: declared %s\n" id (tstring genty) id (tstring declty);
      (* At this point we know that the declared and inferred types are consistent.
         Now we must check that the declared type is identical to or a specialization of the inferred type.
      *)
      if arity genty < arity declty then 
         error id userty inferty true;
       (* *)
       ()
      with 
      Failure message -> error id _userty inferty false
             
in  Mapping.iterpairs checkDecl userenv;
    match !errors with
    | []   -> ()
    | errs -> 
      List.iter 
        (fun (id, uty, infty, toogeneral) ->
         Format.printf "** inferred: %s: %s\n** %sdeclared: %s: %s at %s\n"
             id (tstring infty) 
             (if toogeneral then "not specialized by\n** " else "") 
             id (tstring uty) (lstring(map |?| id));
         )
        errs;
        typeError (Format.sprintf "%seclared and inferred types cannot be reconciled." 
                                  (if List.length errs>1 then "some d" else "d")) inferenv
 


                 


















































































