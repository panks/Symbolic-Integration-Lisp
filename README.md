Symbolic-Integration-Lisp
=========================

Symbolic Integration in Lisp: Lisp code to find indefinite integral, of a given function f(x), incorporates 'Integration by part' and 'Integration by substitution'.

## Usage:

###Input --
 -  All the inputs are in prefix notation e.g. (x*4) becomes 
    (* x 4)
 -  Even though the notaion is prefix, the function is broken in into 'elementry' blocks (i.e. blocks of 3 elements) and a list is dedicated to each block
    e.g. : log ( Sin ( 4 * x)) becomes 
    (log sin ( * 4 x))

###Output --
 - Would be in the same formal as described above




(* Substitution methoda: *)
                / 
 TO instegrate  \ (F( g(x)) g(x) dx   enter like :: (top '(* (sin (* 0.5 (^ x 2))) (x)))
                /
 TO use UV rule functio to be integrated first only!!                 
                

TODO :
