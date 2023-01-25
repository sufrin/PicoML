/*
        Actions (of type ACT) are used to denote the built-in
        effect-inducing procedures.  The data type is declared here;
        the action semantics are specified in the Action module of
        the implementation.

        The intention is to provide something along the lines of
        the (monadic) I/O implementation of Haskell.
        
        InputHandle and OutputHandle are built-in abstract types declared in the 
        prelude.
        
*/

data ACT a = 
        _PutStr         (OutputHandle,  [Char])
     |  _Put            (OutputHandle,  b)
     |  _GetByte        (InputHandle)
     |  _GetLine        (InputHandle)
     |  _GetContents    (InputHandle)
     |  _Echo           (Bool)
     |  _FindInput      ([Char])
     |  _FindOutput     ([Char])
     |  _CloseInput     (InputHandle)
     |  _CloseOutput    (OutputHandle)
     |  _Exit           (Bool)
     /* "Monadic" operator representations */
     |  _Return a
     |  _AndThen(ACT b, b -> ACT a)
     |  _OrElse(ACT a,  b -> ACT a)
     ;

data IOException = Eof | FindInput [Char] | FindOutput [Char];
    
let     
     putStr:        [Char] -> ACT();
     put:           b      -> ACT();
     hputStr:       OutputHandle -> [Char] -> ACT();
     hput:          OutputHandle -> b -> ACT();
     hputStrLn:     OutputHandle -> [Char] -> ACT();
     putStrLn:      [Char] -> ACT();
     
     getByte:       ACT (Num);
     getLine:       ACT [Char];
     hgetByte:      InputHandle -> ACT (Num);
     hgetLine:      InputHandle -> ACT [Char];
     hgetContents:  InputHandle -> ACT [Char];
     
     
     findOutput:    [Char] -> ACT(OutputHandle);
     findInput:     [Char] -> ACT(InputHandle);
     closeInput:    InputHandle  -> ACT();
     closeOutput:   OutputHandle -> ACT();
     
     putStr  s     = _PutStr(stdout, s);
     put     v     = _Put(stdout, v);
     hputStr h s   = _PutStr(h, s);
     hput    h v   = _Put(h, v);
     echo    b     = _Echo(b);
     getByte       = _GetByte(stdin);
     getLine       = _GetLine(stdin);
     hgetByte  h   = _GetByte(h);
     hgetLine  h   = _GetLine(h);
     hgetContents h = _GetContents(h);
     
     findInput    path = _FindInput (path);
     findOutput   path = _FindOutput (path);
     closeInput   h    = _CloseInput h;
     closeOutput  h    = _CloseOutput h;

     exit:         Bool -> ACT ();
     exit ok       = _Exit ok;
     
     /* "Monadic" operators */
     return:       a -> ACT a;
     (>>>):        (ACT a,  a->ACT b)  -> ACT b; /* Haskell's >>= */
     (>>):         (ACT (), ACT b)     -> ACT b;
     (>?>):        (ACT a, b -> ACT a) -> ACT a; /* Exception handling: b is the "exception" type */
     return a      = _Return a;
     act >>> cont  = _AndThen (act, cont);
     act >?> alt   = _OrElse  (act, alt);
     act1 >> act2  = act1 >>> ⟨ () -> act2 ⟩;
     
     /* Synthetic */
     putStrLn s    = putStr s >> putStr "\n";
     hputStrLn h s = hputStr h s >> hputStr h "\n";
     
     /* Idioms */
     repeatedly: ACT () -> ACT ();
     repeatedly act = act >> repeatedly act;
     
     seq: [ACT a] -> ACT [a];
     seq = ⟨ []        -> return []
           | act::acts -> act >>> ⟨ ans -> seq acts >>>  ⟨ answers -> return (ans::answers) ⟩ ⟩ 
           ⟩;
                    
     
     doSeq: [ACT()] -> ACT();
     doSeq = foldr (>>) (return ());
     
     
     /* Haskellish ''perform action right now'' operator
        WARNING: this potentially wrecks referential transparency,
        but is a way of (for example) enabling input and output to be performed
        in a dynamic context other than at the top-level -- which is where
        ACTions are normally executed.
     */
     unsafePerform: ACT a -> a;
     unsafePerform = __unsafePerformAction;
     
     trace: @a . [Char] -> a -> a;
     trace s a = unsafePerform(putStr s>>put a>>putStr "\n">>return a);;
     














