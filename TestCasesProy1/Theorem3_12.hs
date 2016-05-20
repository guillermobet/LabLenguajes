import Term
import Sust
import Functions
import Theorems

verify = let theorem = ( neg (neg p) === p ) in
         proof theorem
         >>=
         statement 3.11 with (p =: p) using lambda z (neg z)
         >>=
         statement 3.9 with (q, p <==> neg q =: p, q) using lambda z (z)
         >>=
         statement 3.2 with (neg q =: q) using lambda z (neg q <==> z)
         >>=
         statement 3.1 with (neg q, neg q, p =: p, q, r) using lambda z (z)
         >>=
         statement 3.6 with (neg q =: q) using lambda z (z)
         >>=
         done theorem
