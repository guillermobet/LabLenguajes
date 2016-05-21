import Term
import Sust
import Functions
import Theorems

verify = let theorem = ( p \/ q === p \/ neg q <==> p ) in
         proof theorem
         >>=
         statement 3.4 with (p \/ q =: p) using lambda z (z)
         >>=
         statement 3.28 with (p =: p) using lambda z (p \/ q <==> z)
         >>=
         statement 3.27 with (neg p =: r) using lambda z (z)
         >>=
         statement 3.201 with (q, p =: p, q) using lambda z (p \/ z)
         >>=
         statement 3.27 with (neg q, p =: q, r) using lambda z (z)
         >>=
         statement 3.26 with (p =: p) using lambda z (p \/ neg q <==> z)
         >>=
         done theorem
