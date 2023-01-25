(*****************************************************************************

        Command-line interface.
                
******************************************************************************)

open Top
open Syntax
open Parse
open Parsing
open Format
open Lexing

let  ignoreFirst = ref false
and  script     = ref false

let rec interpretstream  interactive  (instream, realpath) =
  (* Format.printf "interpretstream %b %s\n" interactive realpath; *)
  let prompt = if interactive then ": " else "" 
  (* and lexbuf = Ulexing.from_utf8_channel instream *)
  and encoding = ref Ulexing.Utf8 in
  let lexbuf = Ulexing.from_var_enc_stream encoding (Stream.of_channel instream)

  and going  = ref true
  in
  let parser = MenhirLib.Convert.Simplified.traditional2revised 
                 (if interactive || !ignoreFirst then Parse.interact else Parse.phrase) 
  in  let lexer = Scanner.getLexer lexbuf realpath prompt
  in  if !ignoreFirst then 
      ( (* picoML can be used for scripts *)
        ignoreFirst := false;
        let _ = input_line instream in ()
      );
      while !going do 
        try
          match parser lexer with
          | Eof    -> going := false;
          | phrase -> Top.process phrase
        with
        |    Failure msg -> 
             let ln, cn, fn = Scanner.getLocation lexbuf
             in  eprintf "Syntax error: %s at %d.%d in %s@." msg ln cn fn
        
        |    Sys.Break  -> 
               eprintf "\n[Interrupted]\n%s@?" prompt
      done;
      Scanner.endFile()

and usefile path =
try (* Format.printf "usefile %s\n" path; *)
    match path with
    | "-"   ->  interpretstream true (stdin, "stdin")
    | "-v"  ->  eprintf "%s@." Version.version; exit 0
    | "+t"  ->  Trace.tracing := false
    | "-t"  ->  Trace.tracing := true
    | "-s"  ->  Top.typeShow  := false
    | "+s"  ->  Top.typeShow  := true
    | "-x"  ->  Top.typeCheck := false
    | "+x"  ->  Top.typeCheck := true
    | "-F"  ->  ignoreFirst   := true
    | "-f"  ->  
                ignoreFirst  := true;
                Top.typeShow := false;
                script := true
    | ""    ->  ()
    | _     ->  
    match String.get path 0 with
    | '-' -> eprintf "Usage: picoml <arg>* [--  <literals>] or #use \"<arg>\"\n<arg> is a path  (default extension .pml), \"-\" (stdin), or one of \n\
-v -- show version\n\
-f -- run next file in script mode (-s -F) and terminate\n\
-F -- ignore first line of next file (assume it's a shell script)\n\
-x -- execute without typechecking\n\
-s -- don't show types\n\
-t -- trace typechecking\n\
+[xst] -- undoes -[xst]\n\
-- <literals> -- binds variable argv:[[Char]] to the list of literals@."
    |  _  -> Format.print_flush(); interpretfile (Scanner.relativePath path); ignoreFirst := false
with 
    Sys_error why -> eprintf "[[ %s ]]@." why 
        
and interpretfile path = interpretstream false (openfile path)

and openfile path = 
    try  (open_in (path^".pml"), path^".pml") 
    with Sys_error _ -> 
         (open_in path, path)


let rec main argv =
    match argv with
    | []        -> (usefile "-")
    | "--"::ps  -> (usefile "-")
    | p   ::ps  -> (if !script then usefile p else (usefile p; main  ps))
   
let _ = begin
    Format.set_margin 80;
    Sys.catch_break true; 
    Utils.fileuse := usefile;
    Utils.setDeclareNotations Scanner.declareNotations;
    main(List.tl(Array.to_list(Sys.argv)))
end





































