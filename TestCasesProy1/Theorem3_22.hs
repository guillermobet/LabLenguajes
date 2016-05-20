import Term
import Sust
import Functions
import Theorems

verify = let theorem = ( neg p === p <==> false ) in
         proof theorem
         >>=
         statement 3.6 with (neg p, p =: p, q) using lambda z (z)
         >>=
         statement 3.1 with (p, neg p =: q, r) using lambda z (z)
         >>=
         statement 3.2 with (neg p =: q) using lambda z (p <==> z)
         >>=
         statement 3.15 with (p =: p) using lambda z (p <==> z)
         >>=
         done theorem
