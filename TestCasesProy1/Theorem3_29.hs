import Term
import Sust
import Functions
import Theorems

verify = let theorem = ( p \/ true === true ) in
         proof theorem
         >>=
         statement 3.3 with (p =: p) using lambda z (p \/ z)
         >>=
         statement 3.27 with (p, p =: q, r) using lambda z (z)
         >>=
         statement 3.3 with (p \/ p =: p) using lambda z (z)
         >>=
         done theorem
