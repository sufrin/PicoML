/* 
        PicoML input library 
        
        This module is a complement to the Actions module which
        provides facilities akin to those provided by Haskell's IO
        type. It provides a useful means of experimenting without
        having to master that type or its monadic setting.
                        
        stdin: Bool -> [Char]
        stdin echoing =
              A list of characters read from the standard input stream. 
              If the parameter is False, then echoing is suppressed, 
              no line-reconstruction is done by the operating system, and
              characters are made available as they are typed.
              If the parameter is True, then the usual mode of terminal
              input is used, with characters being made available whenever
              a newline is typed. In all cases Control-D terminates the input.
                
        
        file: [Char] -> Opt[Char]
        file path =
              Some chars, if the file named by path exists and contains chars
              None otherwise
              
              The file is not read all at once, but is "streamed" lazily 
              on demand, and closed at its end.
              
*/

/*
        These functions are implemented by cheating a little:
        the interpreter primitives __stdin and __file return
        pseudo-functions (``generators'') that need to be called
        to input each character. The only way we can make sure
        that they are called exactly the right number of times
        is to compare their results with the primitive ``eof''
        of type Char. The streamify function deals with the details.
        
        The file primitive returns: Some(generator, closer) or None

*/


let     stdin echoing = streamify $ __stdin echoing
and     file path =      
        ⟨ None               -> None
        | Some (next, close) -> Some $ streamify next close
        ⟩ $ __file path
               
               
where   streamify: (()->Char) -> ([Char]->[Char]) ->[Char];
        streamify next close = 
            let rec stream () = 
                let ch = next() 
                in  if ch=eof then close [] else ch::stream ()
            in stream ()
            
end




