import Term
import Sust
import Functions
import Theorems

verify = let theorem = ( p !<==> q === r <==> (p <==> (q !<==> r)) ) in
         proof theorem
         >>=
         statement 3.14 with (p =: p) using lambda z (z)
         >>=
         statement 3.6 with (neg p <==> q, r =: p, q) using lambda z (z)
         >>=
         statement 3.14 with (p =: p) using lambda z ((r <==>  r) <==> z)
         >>=
         statement 3.1 with (r, r, p !<==> q =: p, q, r) using lambda z (z)
         >>=
         statement 3.2 with (r, p !<==> q =: p, q) using lambda z (r <==> z)
         >>=
         statement 3.19 with (p =: p) using lambda z (r <==> z)
         >>=
         done theorem
