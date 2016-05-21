import Term
import Sust
import Functions
import Theorems

verify = let theorem = ( q === (p !<==> q) <==> neg p ) in
         proof theorem
         >>=
         statement 3.6 with (q, neg p =: p, q) using lambda z (z)
         >>=
         statement 3.1 with (neg p, neg p, q =: p, q, r) using lambda z (z)
         >>=
         statement 3.2 with (neg p =: p) using lambda z (neg p <==> z)
         >>=
         statement 3.1 with (neg p, neg p =: p, r) using lambda z (z)
         >>=
         statement 3.14 with (p =: p) using lambda z (z <==> neg p)
         >>=
         done theorem
