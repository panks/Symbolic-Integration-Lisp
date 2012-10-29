Symbolic-Integration-Lisp
=========================

Symbolic Integration in Lisp: Lisp code to find indefinite integral, of a given function f(x). Incorporates 'Integration by part' and 'Integration by substitution'.
This project contains two files:
- main.lisp -- Contains the main lisp code required for integration
- diff.lisp -- Contains code to provide differentaion of fucntions required for 'Integration by part'

## Usage:

###Input --
 -  All the inputs are in prefix notation e.g. (x*4) becomes 
```lisp    
(* x 4)
```
 -  Even though the notaion is prefix, the function is broken in into 'elementry' blocks (i.e. blocks of 3 elements) and a list is dedicated to each block
    e.g. : log ( Sin ( 4 * x)) becomes 
```lisp
    (log sin ( * 4 x))
```
 - Substitution method: To  integrate  (F( g(x)) g(x)) dx input should be like: (top '(* (sin (* 0.5 (^ x 2))) (x)))
 - Integration by part method: Function to be integrate should in place before the other. 
   e.g. ((Sin x) (x ^ 2)) is preferred over ((x ^ 2) (Sin x))

###Output --
 - Would be in the same formal as described above



####Authors:
Vageesh DC (vageesh.dc@gmail.com)
Pankaj Kumar (me@panks.me)
