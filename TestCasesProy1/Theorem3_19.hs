import Term
import Sust
import Functions
import Theorems

verify = let theorem = ( (p !<==> q) <==> r === p <==> (q !<==> r) ) in
         proof theorem
         >>=
         statement 3.16 with (p =: p) using lambda b (b <==> r)
         >>=
         statement 3.14 with (q, p =: p, q) using lambda a (a <==> r)
         >>=
         statement 3.2 with (neg q =:q) using lambda a (a <==> r)
         >>=
         statement 3.1 with (neg q =:q) using lambda a (a)
         >>=
         statement 3.14 with (q, r =:p, q) using lambda a (p <==> a)
         >>=
         done theorem
