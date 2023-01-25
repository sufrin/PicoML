let x = "this is a very long lexeme\
\and I am going to apply it\
\and I am going to apply it\
\and I am going to apply it\
\and I am going to apply it\
\and I am going to apply it\
\and I am going to apply it\
\and I am going to apply it\
\and I am going to apply it\
\to something or other on line"
4;;
/*
--- and it gets the location of the start of the string incorrect because
--- the symbol has been read over several lines. One day I may give myself
--- the luxury of making this more precise, but for the moment I'll locate
--- the end of the string as precisely as I can. 
*/
let y = "foobaz is very good for you" 4;;



