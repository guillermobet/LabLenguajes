import Term
import Sust
import Functions
import Theorems

verify = let theorem = ( neg p <==> p === false ) in
         proof theorem
         >>=
         statement 3.14 with (p =: q) using lambda z (z)
         >>=
         statement 3.101 with (p =: q) using lambda z (z)
         >>=
         statement 3.3 with (p =: p) using lambda z (neg z)
         >>=
         statement 3.8 with (p =: p) using lambda z (z)
         >>=
         done theorem
