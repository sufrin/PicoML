/* 
        PicoML input library 
        
        This module is a complement to the Action module which
        provides facilities akin to those provided by Haskell's IO
        type. It provides a useful means of experimenting without
        having to master that type or its monadic setting.
                        
        tty: Bool -> [Char]
        tty echoing =
              A list of characters read from the standard input
              stream.  If the parameter is False, then -- providing
              stdin is a terminal -- echoing is suppressed,  no
              line-reconstruction is done by the operating system,
              and characters are made available as they are typed.
              If the parameter is True, then the usual mode of
              terminal input is used, with characters being made
              available whenever a newline is typed. In all cases
              Control-D terminates the input.
             
        
        file: [Char] -> Opt[Char]
        file path =
              Some chars, if the file named by path exists and
              contains chars; None otherwise.

              The file is not read all at once, but is "streamed"
              lazily on demand, and closed at its end. The underlying
              Unix file descriptor stays open until the last character
              of the file has been consumed. If an application fails to
              consume all the characters, then a space-leak will ensue.
              
*/


let     tty:  Bool   -> [Char];
        file: [Char] -> Opt[Char];
        tty  = __stdin;
        file = __file            
end




