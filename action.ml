(*      
        Semantics of the built-in I/O actions of picoML
*)

open Semantics
open Mapping


let unpack2 args = match force args with PairVal(v, w) -> (force v, force w)
    
exception IOException of value
let ioException v = raise (IOException v)

let eof = Const "Eof"
    
let rec perform scont fcont = function
| Inject ("_Put",      args)   -> 
  let (_, v) = unpack2 args in 
      print_val v; 
      Format.print_flush(); 
      scont @@ UnitVal
      
| Inject ("_PutStr",   args)   -> 
  let (_, v) = unpack2 args in 
      print_string v; 
      Format.print_flush(); 
      scont @@ UnitVal
      
| Inject ("_Echo",  v)   -> 
      (match force v with BoolVal echo -> Utils.echo echo | _ -> Utils.echo true);  
      scont @@ UnitVal

| Inject ("_GetByte", chan)   ->  
  (      let Opaque(_, `InChan chan) = force chan
         in  
             let result = try Some (input_byte chan) with _ -> None
             in match result with
             | Some v -> scont (NumVal (Q.of_int v))
             | None   -> fcont eof    
             
  )

| Inject ("_GetLine", chan) ->  
  (      let Opaque(_, `InChan chan) = force chan
         in  
             let result = try Some (input_line chan) with _ -> None
             in match result with
             | Some v -> scont (mkString v)
             | None   -> fcont eof    
  )

  (* The Generator branch of a Delay is used here
     to store an (Ocaml) generator function that's called 
     to generate on demand the next element of the list of characters. 
  *)
| Inject ("_GetContents", chan) ->  
  (      let Opaque(_, `InChan chan) = force chan in
         let gen = Utf8string.from_channel chan
         in  scont @@ listify false gen                  
  )

| Inject ("_AndThen", args) -> 
  let (first, cont) = unpack2 args in      
      perform (apply scont fcont "_AndThen" cont) fcont first

| Inject ("_OrElse", args) ->  
    let (first, alt) = unpack2 args in
        perform scont (apply scont fcont "_OrElse" alt) first
  
| Inject ("_Return", result) -> scont @@ result

| Inject ("_FindInput", path) ->
  (
         let path' = force_string @@ force path in
         try  let chan = open_in path' in 
              scont @@ Opaque("InputHandle", `InChan chan)
         with Sys_error s -> fcont @@ Inject("FindInput",  path) 
  )
         
| Inject ("_CloseInput", chan) ->
  (      let Opaque(_, `InChan chan) = force chan
         in close_in chan; 
            scont @@ UnitVal
  )
         
| Inject ("_FindOutput", path) ->
  (
         let path' = force_string @@ force path in
         try let chan = open_out path' in 
                 scont @@ Opaque("OutputHandle", `OutChan chan)
         with Sys_error s -> fcont @@ Inject("FindOutput", path)
  )
         
| Inject ("_CloseOutput", chan) ->
  (      let Opaque(_, `OutChan chan) = force chan
         in close_out chan; 
            scont @@ UnitVal
  )
  
| Inject ("_Exit", ok) ->
  (match force ok with BoolVal ok ->  if ok then exit 0 else exit 1 | _ -> scont @@ UnitVal);  
  scont @@ UnitVal

         
| Inject (name, _) -> failwith (Format.sprintf "Unimplemented ACT %s" name)
     
and apply scont fcont name fn arg =
  let next = match fn with
  | FunVal(env', bv, body)       -> eval (env' <+> (bv|->arg)) body
  | CaseFunVal(env', cases, loc) -> firstCase loc env' arg cases        
  | other -> failwith (Format.sprintf "dynamic type error performing %s continuation" name)
  in  
    perform scont fcont @@ force next

let performAct: value -> value = perform (fun v->v) ioException 
  
















