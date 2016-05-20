import Term
import Sust
import Functions
import Theorems

verify = let theorem = (p <==> p) <==> (q <==> q) === true in
         proof theorem
         >>=
         statement 3.3 with (p =: p) using lambda z (z <==> (q <==> q))
         >>=
         statement 3.3 with (q =: p) using lambda z (true <==> z)
         >>=
         statement 3.3 with (true =: p) using lambda z (z)
         >>=
         done theorem
