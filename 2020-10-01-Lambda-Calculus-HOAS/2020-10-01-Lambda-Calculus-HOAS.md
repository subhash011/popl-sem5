# Lambda Calculus: Higher order abstract syntax (HOAS).
Deadline :: Oct 15, 2020

In the last lab we defined lambda calculus expressions using a ML data
type. One of the problem with that definition is that the interpreter
has to worry about variable capture and bound variable renaming. When
implementing substitution based systems like lambda calculus in a
functional programming language like ML, we can use a technique called
the _Higher Order Abstract Syntax_ (HOAS) for short. The main idea is
the following: Instead of representing a lambda as a tuple
`string * lambda`, we represent it as a function `hoas -> hoas` (see the
definition below).

```
(* normal representation of lambda calculus *)
datatype lam = V of string
             | A of lam    * lam
             | L of string * lam

(* HOAS representation of lambda calculus *)

datatype hlam = HV   of string
              | HA   of hlam * hlam
              | HL   of hlam -> hlam

(* as opposed to L of string * lam *)
```

The main advantage of this representation is clean and fast
β-reductions without worrying about free variable capture --- a term
like `λ x . M` is represented by a term like `HL (fun x => MH)` (where
MH denote the HOAS representation of M) so β-reducing `(λ x . M) N` is
just function application.

In this assignment we work with functions related to the HOAS syntax

1. Define the substitution function `subst : string * hlam -> hlam ->
   hlam`. The expression `subst ("x",u) e` should replace every free
   occurance of the variable `"x"` in `e` with `u`. The tricky case is
   how does one handle the `HL f` case.  **HINT** `subst (x,u) (HL f)`
   is `HL fp` where `fp inp` is essentially `f inp` but for
   substituting `x` by `u`.


2. Notice that the substitution did not have to worry about the
   variable bound by the lambda and is simpler than the one defined
   for standard lambda calculus representation. However building a
   lambda term is difficult. Define the function `abstract : string ->
   hlam -> hlam` that essentially builds the HOAS term for `λ x . M`,
   i.e. if `mh : hlam` is the HOAS representation of `M`, then
   `abstract "x" mh` should give the HOAS representation for `λ x
   . M`. **HINT** The HOAS for `λ x . M` is given `HL f`, where `f` is
   that function that takes a HOAS `nh : hlam` and gives the HOAS term
   obtained by substituting "x" with `nh` in `mh`. Use part 2 to
   complete the assignment.

3. Define a function `freshen : hlam -> string` which will generate a
   string that does not occur free in its input. For example, for
   something like `HL (fn t => HA (t , HV "x"))` it can generate any
   string but the string "x". If we have a way to compute the free
   variables of `hlam` one can use that. However, the `freshen` is
   easier. `free : hlam -> list string`. Write a helper function
   `freeP : hlam -> list string` such that for all `t : hlam` `freeP t
   ∩ FV(t) = ∅`. Idea is when one sees `FV(HAb f) ⊂ f (HV "x")`.


4. Give a conversion functions `hoas : lam -> hlam` `syntax : hlam ->
   lam`. In both cases handling the constructor associated with with
   lambda abstraction is difficult.

   For the function `hoas` you need to define the function `abstract :
   string -> hlam -> hlam` where `abstract "x" e` computes the HOAS
   representation of `λ x . M` where `M` is the lambda calculus term
   associated with `e`. Use the substitution fuction here.

   For `syntax`, function when handling (HL f) you will have to cook
   up a variable u and then take that is fresh for `f`. For example if
   the term was HL (fn x => HA (V "t", x)) then generating the
   variable "y" is okey but not "t". You can use the `freshen`
   function here.

## Implement Reduction strategy (Bonus, ungraded)

Implement some of the evaluation strategies given in the Wikipedia
page https://en.wikipedia.org/wiki/Evaluation_strategy. Compare how
convenient it is hoas vs the standard implementation.

[atom]: <https://www.classes.cs.uchicago.edu/archive/2015/spring/22620-1/atom-sig.html>
[sets]: <https://www.classes.cs.uchicago.edu/archive/2015/spring/22620-1/ord-set-sig.html>
