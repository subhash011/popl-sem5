# Unification algorithm.

Deadline: 29 Oct, 2020

The unification algorithm is a recursive algorithm on the structure of
the terms. Recall that a *term* over the signature `S` with variable
set `V`, denoted by `Term(S,V)` is defined inductively as follows.

   - Any constant of `c ∈ S` is a term
   - Any variable `x ∈ S` is a term
   - If `t₁,...,tₙ` are terms and `f ∈ S` is a functional symbol of arity `n` then
   - f (t₁,....,tₙ) is also a term.


The essence of unification of two terms `s` and `t` is the following.

1. Two constants may be unified if they are the same constant.

2. If one of the terms is a variable `x` and the other is the term `t`, then
   one gets a variable assignment then and there.

3. Unification of terms `f(s₁,....,sₙ)` and `g(t₁,...,tₘ)` is possible
   if and only if the function symbol `f` and `g` are the same (thus
   `n = m`) and sᵢ unifies with tᵢ 's respectively. The term `f(s₁,...,sₙ)`
   unifies with `f(t₁,...,tₙ)` under the telescope σ if and only if each
   sᵢ unifies with tᵢ under σ.





However, getting this in practice requires writing the mutually
recursive generalisations.

1. `unify : telescope -> (term, term) -> option telescope`.  Given a
   telescope `σ` and terms `s` and `t`, it computes an extension `σ₀`
   of `σ` (if possible) that unifies `s[σ]` and `t[σ]`. Recall our
   notation that `s[σ]` is the term obtained by substituting all the
   variable assignments of `σ` in `s`.

2. `unifyList : telescope -> (term, term) list -> option
   telescope`. This function simultaneously unifies each of the terms
   under the given telescope.
