import Term
import Sust
import Functions
import Theorems

verify = let theorem = ( p \/ (q \/ r) === (p \/ q) \/ (p \/ r) ) in
         proof theorem
         >>=
         statement 3.26 with (p =: p) using lambda z (z \/ (q \/ r))
         >>=
         statement 3.25 with (p, q \/ r =: q, r) using lambda z (z)
         >>=
         statement 3.25 with (p =: p) using lambda z (p \/ z)
         >>=
         statement 3.24 with (p =: p) using lambda z (p \/ (z \/ r))
         >>=
         statement 3.25 with (q, p =: p, q) using lambda z (p \/ z)
         >>=
         statement 3.25 with (p \/ r =: r) using lambda z (z)
         >>=
         done theorem
