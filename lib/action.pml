/*
        Actions (of type ACT) are used to denote the built-in
        effect-inducing procedures.  The data type is declared here;
        the action semantics are specified in the Action module of
        the implementation.

        The intention is to provide something along the lines of
        the (monadic) I/O implementation of Haskell.
        
        InputHandle and OutputHandle are abstract types: in the
        present implementation this does not need to be declared
        (indeed it cannot be).
        
*/

data ACT a = 
        _PutStr         (OutputHandle,  [Char])
     |  _Put            (OutputHandle,  b)
     |  _GetCh          (InputHandle)
     |  _GetLine        (InputHandle)
     |  _Echo           (Bool)
     |  _FindInput      ([Char])
     |  _FindOutput     ([Char])
     |  _CloseInput     (InputHandle)
     |  _CloseOutput    (OutputHandle)
     /* "Monadic" operator representations */
     |  _Return a
     |  _AndThen(ACT b, b -> ACT a)
     ;
     
let     
     putStr:        [Char] -> ACT();
     put:           b      -> ACT();
     hputStr:       [Char] -> OutputHandle -> ACT();
     hput:          b      -> OutputHandle -> ACT();
     
     getCh:         ACT [Char];
     getLine:       ACT [Char];
     hgetCh:        InputHandle -> ACT [Char];
     hgetLine:      InputHandle -> ACT [Char];
     
     
     findOutput:    [Char] -> ACT(Opt OutputHandle);
     findInput:     [Char] -> ACT(Opt InputHandle);
     closeInput:    InputHandle  -> ACT();
     closeOutput:   OutputHandle -> ACT();
     
     putStr  s     = _PutStr(stdout, s);
     put     v     = _Put(stdout, v);
     hputStr s  h  = _PutStr(h, s);
     hput    v  h  = _Put(h, v);
     echo    b     = _Echo(b);
     getCh         = _GetCh(stdin);
     getLine       = _GetLine(stdin);
     hgetCh    h   = _GetCh(h);
     hgetLine  h   = _GetLine(h);
     
     findInput    path = _FindInput (path);
     findOutput   path = _FindOutput (path);
     closeInput   h    = _CloseInput h;
     closeOutput  h    = _CloseOutput h;
     
     /* "Monadic" operators */
     exit:         ACT();
     return:       a -> ACT a;
     (>>>):        (ACT a,  a->ACT b) -> ACT b;
     (>>):         (ACT (), ACT b)    -> ACT b;
     exit          = _Return();
     return a      = _Return a;
     act >>> cont  = _AndThen (act, cont);
     act1 >> act2  = act1 >>> λ { () -> act2 };; 
  






