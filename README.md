Symbolic-Integration-Lisp
=========================

Symbolic Integration in Lisp: Lisp code to find indefinite integral, of a given function f(x). Incorporates 'Integration by part' and 'Integration by substitution'.
This project contains two files:
- main.lisp -- Contains the main lisp code required for integration
- diff.lisp -- Contains code to provide differentaion of fucntions required for 'Integration by part'

## Usage:
 -  The integration is performed by calling the function 
```
integratetop
```
and is shown below :

###Input --
 -  All the inputs are in prefix notation e.g. (x*4) becomes 
``` 
(* x 4)
```
 -  Even though the notaion is prefix, the function is broken in into 'elementry' blocks (i.e. blocks of 3 elements) and a list is dedicated to each block
    e.g. : log ( Sin ( 4 * x)) becomes 
```
    (integratetop '(log sin ( * 4 x)))
```
 - Substitution method: To  integrate  (F( g(x)) g(x)) dx input should be like: 
```(integratetop 
'(* (sin (* 0.5 (^ x 2))) (x)))
```
 - Integration by part method: Function to be integrate should in place before the other. 
   e.g. 
```(integratetop '(* (Sin x) (x ^ 2)))
``` is preferred over 
```(integratetop '(* (x ^ 2) (Sin x)))
```
 - Pass all the lists to 'integratetop' function for integration
   e.g. for integrating x^2 call

```(integratetop '(^ x 2))
```

###Output --
 - Would be in the same formal as described above



###Authors:
Vageesh DC (vageesh.dc@gmail.com) <br />
Pankaj Kumar (me@panks.me)
