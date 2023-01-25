---
--- Test notation declarations
---

notation
        left     9  +++ *** × 
and
        right    8  //
and
        leftdata 8  ::: `foo`
and
        right    plus
and
        rightdata ****
and 
        constant **
        ;;

data Y a = ** | a **** (Y a);
data Z a = a `foo` a;

x *** y = (x,y);
x +++ y = (x,y);     
x // y = (x,y);                  
f(x `foo` y) = (x,y);
a plus b = a+b              




