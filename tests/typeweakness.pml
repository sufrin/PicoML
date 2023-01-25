/*
        
        The following examples (used to) show the weakness of the
        PML type inference algorithm when used with "advisory"
        declarations.

        When mapf is declared as [a]->[b] or [a]->[Num] or [Num]->[b]
        it used to generate a spuriously general type for the result
        of outer (mapf), unless outer has its own declaration. I
        concluded that an inner advisory /declaration/ must not be
        generic, and incorporated appropriate restrictions in the
        typechecker.

        The problem for us was with the scope of the type variables
        used in advisory declarations. It's not a theoretical
        problem, and the solution is

        1. to use a straightforward (but laborious) transformation
        of the implementation of type substitution to use a
        type-variable->type-unknown environment, and

        2. to stop interpreting all types with type variables in
        them as (implicit) polytypes.  We can't really be bothered
        to do this right now, since no expressive power is added
        to the language by doing so.
               
        Ocaml gets this kind of thing consistently right. It can do so
        straightforwardly since type hints are /part/ of its declarations
        rather than being /supplementary/.

        let outer f   = let mapf = List.map f in mapf;;
        (* val outer : ('a -> 'b) -> 'a list -> 'b list = <fun> *)
      
        let outer (f: 'a-> 'b)  = let mapf = List.map f in mapf;; 
        (* val outer : ('a -> 'b) -> 'a list -> 'b list = <fun> *)
        
        let outer f  = let mapf: 'a list-> 'b list = List.map f in mapf;; 
        (* val outer : ('a -> 'b) -> 'a list -> 'b list = <fun> *)
        
        let outer f  = let mapf: int list-> 'b list = List.map f in mapf;; 
        (* val outer : (int -> 'b) -> int list -> 'b list = <fun> *)
        
        let outer f = let mapf: 'c list -> 'd list = List.map f in mapf;;
        (* val outer : ('c -> 'd) -> 'c list -> 'd list = <fun> *)
          
*/


let 
    outergood1 f = 
    let
      mapf = map f
     in 
      mapf;;
let 
    outergood2 f = 
    let
      mapf: [Num]->[Num];
      mapf = map f
     in 
      mapf;;
      
let outergood3: (a->b) -> [a]->[b];
    outergood3 f = 
    let
      --- mapf: [a]->[b];
      mapf = map f
     in 
      mapf;;
      
--- another weakness is that (poly) type variable names are normalized to a,b,c ... when declarations are installed
--- this is only negligibly inconvenient, since it only affects the reporting of interactive declarations
      
let outergood3a: (alpha->beta) -> [alpha]->[beta];
    outergood3a f = 
    let
      --- mapf: [a]->[b];
      mapf = map f
     in 
      mapf;;
                  
let 
    outerbad3 f = 
    let
      mapf: [a]->[b];
      mapf = map f
     in 
      mapf;;

let 
    outerbad4 (f: a->b) = 
    let
      mapf: [a]->[b];
      mapf = map f
     in 
      mapf;;

let 
    outergood4 (f: a->b) = 
    let
      mapf = map f
     in 
      mapf;;
      
let 
    outergood5 (f: Num->Num) = 
    let
      mapf = map f
     in 
      mapf;;

let 
    outerbad3 f = 
    let
      mapf: [a]->[b];
      mapf = map f
     in 
      mapf;;

let 
    outerbad4 (f: a->b) = 
    let
      mapf: [a]->[b];
      mapf = map f
     in 
      mapf;;

let 
    outergood6 (f: a->b) = --- the scope of a, b here should include the [a]->[b] hint below
    let
      --- mapf: [a]->[b];
      mapf = \( [] -> [] | x::xs-> f x :: mapf xs )
     in 
      mapf;;

let 
    outerbad6 f = 
    let
      mapf: [a]->[b];
      mapf = \( [] -> [] | x::xs-> f x :: mapf xs )
     in 
      mapf;;





