# Basic functions in SML

Deadline: 26th Aug 2020, 23:59 hrs

In this exercise you do some basic functions

1. Write the tri-variate (i.e. 3 variable) versions of `curry` and
   `uncurry` functions.  First step is to write down the type (in the
   comments).

2. Write the functions `fst : 'a * 'b -> 'a` and `snd : 'a * 'b -> 'b`
   that project a tuple into its components.

3. Write the length function for lists `length : 'a list -> int`.

4. Write the `reverse : 'a list -> 'a list` function. Be careful to
   not make it O(n^2)

5. Write a function to compute the nth element in the Fibonacci
   sequence `fib : int -> int`. Be careful for the obvious version is
   exponential.
