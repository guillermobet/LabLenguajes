import Term
import Sust
import Functions
import Theorems

verify = let theorem = ( p \/ false === p ) in
         proof theorem
         >>=
         statement 3.15 with (p =: p) using lambda z (p \/ z)
         >>=
         statement 3.27 with (p, neg p, p =:p, q, r) using lambda z (z)
         >>=
         statement 3.26 with (p =: p) using lambda z (p \/ neg p <==> z)
         >>=
         statement 3.28 with (p =: p) using lambda z (z <==> p)
         >>=
         statement 3.2 with (true, p =: p, q) using lambda z (z)
         >>=
         statement 3.4 with (p =: p) using lambda z (z)
         >>=
         done theorem
