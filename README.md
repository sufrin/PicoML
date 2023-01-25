## PicoML

A polymorphically-typed lazy functional language with I/O actions

**PicoML** was designed as a part of the second year *Programming
Language (Principles and Implementation)* course I used to give in
Oxford around the turn of the millennium. The course covered a host
of topics and, in particular, related abstract machines, operational
semantics, type systems, and various styles of implementation.

I'm afraid I was a little self-indulgent in the design of the
notation, though it can still be recognised as being in the
tradition of Landin's ISWIM. Here are some declarations from its lists
library that will, perhaps, give you a flavour: 

         --- type declarations
         
         (..):      (Num,Num)->[Num];
         (...):     (Num,Num)->[Num];
         (×):       @a,a',b,b'.(a->a', b->b') -> (a,b)->(a', b');
         takeWhile: @a.(a->Bool)->[a]->[a];
         dropWhile: @a.(a->Bool)->[a]->[a];
         split:     @a.(a->Bool)->[a]->([a],[a]);
         lines:     [Char]->[[Char]];
         filter:    @a.(a->Bool)->[a]->[a];

         --- value declarations
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

         --- a fixpoint function
         μ f = f(μ f);

Another point of interest that made the language useful to me during
a course on cryptology is that numbers are represented as (efficiently
implemented) arbitrary precision rationals.

### Actions and the I/O Monad

In forcing myself to construct the implementation here I got a good
deal of insight into  Haskell's I/O Monad that I had -- along with
very many of my peers at the time, it seems -- hitherto found hard
to understand.  It was never the notion of monad that was difficult,
but the interaction between necessarily-lazy evaluations yielding
actions, and their subsequent interpetation "at the top level".
This is only barely covered in the lecture notes found in `picoml.pdf`,
but the detailed interpretation of actions appears as a self-contained
module in `action.ml`. That implementation could be a little more
efficient, but the principles are in place.


**Bernard Sufrin, Oxford, January 2023**