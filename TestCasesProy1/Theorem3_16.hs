import Term
import Sust
import Functions
import Theorems

verify = let theorem = ( p !<==> q === q !<==> p ) in
         proof theorem
         >>=
         statement 3.101 with (p =: p) using lambda a (a)
         >>=
         statement 3.2 with (p =: p) using lambda a (neg a)
         >>=
         statement 3.101 with (q, p =:p, q) using lambda a (a)
         >>=
         done theorem
