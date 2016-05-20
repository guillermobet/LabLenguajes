import Term
import Sust
import Functions
import Theorems

verify = let theorem = p === (q <==> q) <==> p in
         proof theorem
         >>=
         statement 3.5 with (p =: p) using lambda z (z)
         >>=
         statement 3.2 with (p =: p) using lambda z (z <==> q)
         >>=
         statement 3.1 with (q, p, q =: p, q, r) using lambda z (z)
         >>=
         statement 3.2 with (p =: p) using lambda z (q <==> z)
         >>=
         statement 3.1 with (q, p =: p, r) using lambda z (z)
         >>=
         done theorem
