#!/usr/bin/env picoml -s lib/lib -f 
--- 
--- A script can usually be constructed as a single expression 
--- This program consists of two: the first yields a string, the second a composite ACT()
---
"This demonstrates that picoml can run as a script interpreter";
let 
    as=argv;
    thisFile=hd as
in
    putStrLn "The first line of a picoml script follows the usual unix convention, for example:"
    >>
    putStrLn "#!/usr/bin/env picoml -s lib/lib -f "                               
    >>
    putStrLn "The remaining lines consist of one (or more) picoml expressions"
    >>        
    putStrLn "Command line arguments are passed to the script in the variable 'argv'"
    >>        
    putStrLn "Here are the arguments to the current script invocation"
    >>        
    doSeq (map putStrLn argv)                                                        
    >>
    putStrLn "the value of the (last) expression can be an 'exit' ACT()"        
    >> 
    putStrLn "Here is the text of the current script:"
    >>        
    findInput thisFile >>> hgetContents >>> putStrLn    
    >>
    (if hd (rev as) =/= "-x" then return () else exit False)  
end

