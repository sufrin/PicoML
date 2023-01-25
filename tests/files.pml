/*
        File copying
*/ 
--- import "lib/lib";

let copyFile h =
    let rec line n = 
         (hgetLine h >>> putStrLn >> line (n+1))
         >?> 
         \( Eof -> return n )
     in  line 0;;
    
let copy f = 
    (findInput f >>> copyFile >>> put >> putStrLn " lines" >> exit True) 
    >?> 
    \( FindInput s -> putStr "No such file: " >> putStrLn s );;




