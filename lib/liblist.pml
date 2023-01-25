/*
        PicoML lists library  
        
        Notation for functions defined by cases over patterns:
 
        ⟨ pat -> body
        | pat -> body
          ...
        ⟩ 
        
        OR 
        
        λ ( pat -> body
          | pat -> body
            ...
          )
        
        OR (if you find editing with unicode symbols tedious)
        
        \ ( pat -> body
          | pat -> body
            ...
          )
        
        I have used a mix of notations here.
             
*/
    
    --- Signatures can be less general than the inferred types of variables
    --- (and they need not be given at all)
 
    (<*):      @a,b.((b,a)->a,a)->[b]->a;
    foldr:     @a,b.((b,a)->a)->a->[b]->a;
    (*>):      @a,b.(b,(b,a)->b)->[a]->b;
    foldl:     @a,b.((b,a)->b)->b->[a]->b;
    map:       @a,b.(a->b)->[a]->[b];
    len:       @a.[a]->Num;
    take:      @a.Num->[a]->[a];
    drop:      @a.Num->[a]->[a];
    (++):      @a.([a],[a])->[a];
    (!!):      @a.([a],Num)->a;
    ($$):      @a,b,c.(c->b,a->c)->a->b;
    (∘):       @a,b,c.(c->b,a->c)->a->b;
    (⨾):       @a,b,c.(a->c,c->b)->a->b;
    flip:      @a,b,c.((c,b)->a)->(b,c)->a;
    curry:     @a,b,c.((c,b)->a)->c->b->a;
    uncurry:   @a,b,c.(c->b->a)->(c,b)->a;
    rev:       @a.[a]->[a];
    (&&):      (Bool,Bool)->Bool;
    (||):      (Bool,Bool)->Bool;
    not:       Bool->Bool;
    (=/=):     @a.(a,a)->Bool;
    zipWith:   @a,b,c.((c,b)->a)->[c]->[b]->[a];
    (|||):     @a,b.([b],[a])->[(b,a)];
    (..):      (Num,Num)->[Num];
    (...):     (Num,Num)->[Num];
    (×):       @a,a',b,b'.(a->a', b->b') -> (a,b)->(a', b');
    takeWhile: @a.(a->Bool)->[a]->[a];
    dropWhile: @a.(a->Bool)->[a]->[a];
    split:     @a.(a->Bool)->[a]->([a],[a]);
    lines:     [Char]->[[Char]];
    filter:    @a.(a->Bool)->[a]->[a];

    --- Definitions of the form «pattern=expression» are implicitly simply recursive 
    --- Mutually recursive definitions should be declared
    --- explicitly to be simultaneously recursive «rec def1 and def2 and ...»
            
    (res *> (⊗))  = let f res = ⟨ [] -> res | x::xs -> f (res⊗x) xs ⟩ in f res;
    foldl (⊗) res = (res *> (⊗));
    
    ((⊗) <* res)  = let f = ⟨ [] -> res | x::xs -> x ⊗ f xs ⟩ in f;
    foldr (⊗) res = ((⊗) <* res);
    
    map f = λ(  []    -> [] 
             |  x::xs ->  f x :: map f xs
             );
                
    len = λ ( []      -> 0 
            | _ :: xs -> 1+len xs 
            );

    take n xs =
        if null xs then [] else 
        if n=0     then [] else
           hd xs :: take (n-1) (tl xs) ;  
           
    drop n xs = 
        if n=0     then xs else 
        if null xs then xs else 
           drop (n-1) (tl xs);

    (++) = ⟨ ([], ys)    -> ys
           | (x::xs, ys) -> x::(xs++ys)                
           ⟩;
                   
    xs !! n = 
        if null xs then fail "!!" else 
        if n=0     then hd xs else tl xs !! (n-1);
            
    /* compose  */
    (f $$ g) x = f $ g x;
    (f ∘ g)  x = f $ g x;
    (f ⨾ g)  x = g $ f x;
       
    /* combinators */
    flip  f          = ⟨ (x,y) -> f(y,x) ⟩;
    curry f x y      = f(x,y); 
    uncurry f (x, y) = f x y;
    
    /* reverse  */
    rev = [] *> flip (::);
    reverse xs = let rv = \( ([], r) -> r | (x::xs, r) -> rv (xs, x::r)) in rv (xs, []);
    
    /* logic */
    p && q  = if p then q else False;
    p || q  = if p then True else q;
    not x   = if x then False else True; 
    x=/=y   = if x=y then False else True;
    
    zipWith (⊗) xs ys =
    if   null xs && null ys then [] 
    else hd xs ⊗ hd ys :: zipWith (⊗) (tl xs) (tl ys);
    
    (|||) = λ( ([], _) -> []
             | (_, []) -> []
             | (x::xs, y::ys) -> (x,y)::(xs|||ys)
             );
                
    m .. n  = if m>=n then [] else m :: ((m+1) .. n);
    
    m ... n = if m>n then [] else m :: ((m+1) ... n);

    takeWhile p = λ
    ( []    -> []
    | x::xs -> if p x then x::takeWhile p xs else []
    );
    
    dropWhile p = λ
    ( []    -> []
    | x::xs -> if p x then dropWhile p xs else xs
    );

    split p xs = (takeWhile p xs, dropWhile p xs);
    
    filter p =
        ⟨ []    -> []
        | x::xs -> if p x then x::filter p xs else filter p xs
        ⟩;

    lines = 
        ⟨ [] -> []
        | xs -> let (h, t) = split (=/= '\n') xs in
                    h :: lines t                
        ⟩;
        
     (f×g)(x,y)=(f x, g y)















































