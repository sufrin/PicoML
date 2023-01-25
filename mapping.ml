(*      $Id: mapping.ml 150 2019-07-26 15:46:48Z sufrin $
*)

(*
        This module defines  mappings -- representing
        them as association lists.

        The exception Unbound s is raised when an attempt
        is made to look up s in a mapping which doesn't
        bind it.
*)
open Utils

exception Unbound of string

type 'a map = (string*'a) list


(*  Lookup $id$ in $map$ (\verb/|?|/) *)
let rec (|?|): 'a map->string->'a =                 
fun map id ->
match map with
|   (i, t):: map when i=id -> t
|   []                     -> raise (Unbound id)
|   _     :: map           -> map |?| id


(*  Override $map$ with $map'$ (\verb/|+|/)  *)
let (|+|): 'a map->'a map->'a map =                                                
fun map map' -> map' @ map

exception NonDisjoint of string

let checkdisjoint map map' =
    let dom map = List.map fst map in
    let inter = intersection (dom map) (dom map') in  
    match inter with 
    | [] -> ()
    | _ -> 
     raise (NonDisjoint(String.concat ", " inter))


(* Union of disjoint mappings (\verb/|++|/)*)
let (|++|): 'a map->'a map->'a map =                                            
    fun map map' -> 
        checkdisjoint map map'; map' @ map
    
(* Make a maplet (\verb/|->/) *)
let (|->): string->'a->'a map =             
    fun s t -> [(s,t)]

let emptymap: 'a map = []
    


(* Range of a mapping *)
let range: 'a map->'a list =                  
fun map -> remdups(List.map (fun (_, y) -> y) map)
    

(* $map~ f [i|->t; ...] = [i |-> f t; ...]$ *)    
let map: ('a->'a)->'a map->'a map =      
    fun f -> (List.map (fun (i, t) -> (i, f t)))

(* $mappairs ~f [i|->t; ...] = [f~i~t; ...]$ *)
let mappairs: (string->'a->'b)->'a map->'b map = 
    fun f -> (List.map (fun (i, t) -> (i, f i t)))
    

(* $iter ~f~ [i|->t; ...] = (f t; ...)$ *)
let iter: ('a->unit)->'a map->unit =     
    fun f -> List.iter (fun (i, t) -> f t)


(* $iterpairs ~f [i|->t; ...] = (f~i~t; ...)$ *)
let iterpairs: (string->'a->unit)->'a map->unit = 
    fun f -> List.iter (fun (i, t) -> f i t)

let interiorpairs: (bool->string->'a->unit)->'a map->unit = 
    fun f map -> 
    let rec doit = function []           -> ()
                   |        [(i, a)]     -> f false i a
                   |        (i, a)::rest -> f true  i a; doit rest
    in doit map 
     

(* $fromList~f [x1; ... ] = [f x1; ... ]$ *)
let fromList: ('a->string * 'b)->'a list->'b map = 
    List.map
    

(*  $fromLists [i1; ... ] [x1; ...] = [i1|->x1; ... ]$ *)    
let fromLists: string list->'a list->'a map = 
fun names values -> 
    List.fold_right2 (fun i a map -> (i,a)::map) names values []


(*  $tabulate ~f [i1; ... ] = [i1|->f i1; ... ]$ *)
let tabulate: (string->'a)->string list->'a map= 
fun f names -> 
    List.fold_right (fun i map -> (i, f i)::map) names []
    

(* Eliminate redundant entries in a map *)
let simplify: 'a map->'a map = 
fun mapping -> 
    List.map (fun i -> (i, mapping|?|i)) (remdups (List.map fst mapping))  
    
(* Override and simplify mappings *)

let (|+++|): 'a map->'a map->'a map =                                                
fun map map' -> simplify (map' @ map)











