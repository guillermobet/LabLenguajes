import Term
import Sust
import Functions
import Theorems

verify = let theorem = ( (p !<==> q) !<==> r === p !<==> (q !<==> r) ) in
         proof theorem
         >>=
         statement 3.14 with (p =: p) using lambda a (a !<==> r)
         >>=
         statement 3.14 with (neg p <==> q, r =: p, q) using lambda a (a)
         >>=
         statement 3.2 with (neg p =:p) using lambda a (neg a <==> r)
         >>=
         statement 3.9 with (q , neg p =:p, q) using lambda a (a <==> r)
         >>=
         statement 3.2 with (neg q , neg p =:p, q) using lambda a (a <==> r)
         >>=
         statement 3.1 with (neg p , neg q =:p, q) using lambda a (a)
         >>=
         statement 3.14 with (neg q <==> r =:q) using lambda a (a)
         >>=
         statement 3.14 with (q, r =:p, q) using lambda a (p !<==> a)
         >>=
         done theorem
