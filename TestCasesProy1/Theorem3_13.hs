import Term
import Sust
import Functions
import Theorems

verify = let theorem = ( neg false === true ) in
         proof theorem
         >>=
         statement 3.8 with (p =: p) using lambda z (neg z)
         >>=
         statement 3.12 with (true =: p) using lambda z (z)
         >>=
         done theorem
