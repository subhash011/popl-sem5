# Lambda Calculus with let and let-rec

Deadline: 2020-10-08


Consider variants of lambda calculus λ-calculus ⊆ λ-let ⊆ λ-letrec
where the former adds the syntactic sugar of non-recursive `let` and
the latter adds `letrec`.

1. Define abstract syntax for λ-let and λ-letrec as a SML datatype.

2. Write the conversion from these language to that of the plain
   lambda calculus as described in the class. Write the conversion
   process using two functions `unletrec : λ-letrec -> λ-let` and
   `unlet : λ-let -> λ-calculus`

## Mutual recursion (bonus, ungraded).

The fixpoint combinator `Y` gives a solution for a recursive function
definition. What about mutual recursion ? HINT: Pairing is the key.
