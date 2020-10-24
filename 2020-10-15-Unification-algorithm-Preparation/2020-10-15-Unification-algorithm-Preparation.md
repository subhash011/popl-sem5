# Unification algorithm: Preparation

Deadline: 25 Oct 2020, Sunday

The unification algorithm is probably one of the most useful algorithm
available for symbolic processing and is the bed rock of many
algorithms like type inference, Logic resolution etc. In this
assignment we build towards an implementation of the unification
algorithm. First some terminologies.


1. A *signature* is a set `S = {f₁,...,fₙ}` of functional symbols
   where each functional symbol `fᵢ` is associated an arity `nᵢ`.


2. *Constants* of the signature `S` are functional symbol fᵢ's of zero arity
   i.e, `nᵢ = 0`

3. A *term* of the signature `S` denoted by `Term(S)` is defined inductively as follows.

   - Any constant of `c ∈ S` is a term
   - If `t₁,...,tₙ` are terms and `f ∈ S` is a functional symbol of arity `n` then
   - f (t₁,....,tₙ) is also a term.


4. A *term* over the signature `S` with variable set `V`, denoted by `Term(S,V)`
   is defined inductively as follows.

   - Any constant of `c ∈ S` is a term
   - Any variable `x ∈ S` is a term
   - If `t₁,...,tₙ` are terms and `f ∈ S` is a functional symbol of arity `n` then
   - f (t₁,....,tₙ) is also a term.

	Terms over `S` with variable set `V` is just `Term (S ∪ V)` where
	where every element of `V` is assigned an arity 0 (or is treated
	as a constant as far as the building terms are considered).


5. A *telescopic variable assignment* or just a *telescope* is a
   *sequence* (not a set because order matters) Σ of equations of the
   form ` x₁ ≡ t₁,..., xₘ ≡ tₘ` where xᵢ's are variables and tᵢ's are
   terms with the following *telescopic* property:

   - For each index 1 ≤ i ≤ m, none of the variables x₁,...,xᵢ should
	 occur in tᵢ. In other words `Var(tᵢ) ∩ {x₁,...,xᵢ} = ∅`, where
	 Var(tᵢ) denotes the variables occurring in tᵢ

The *unification problem*: Given two terms `t₁` and `t₂` in
`Term(S,V)` as input, compute (if possible) a *telescopic variable
assignment* under which the terms become equal. We will see in class
that the algorithm computes the most general such assignment.

The assignment is a preparatory assignment for building a full fledged
unification algorithm. As a first step we set up the machinary.


1. A signature as we defined above is a set, in implementations we
   capture it as a type. Define a ML signature that captures a
   signature as we just mentioned.

   ```
   signature SIG = sig

   end

   ```

   The contents of this signature should be a type that captures the
   symbols and function that compute the `arity` given the function.
   We also need comparisons on functional symbols so inside the
   signature we need a structure `Ord : ORD_KEY` on the symbol type.
   You will need to assert here that the type `Ord.ord_key` and the
   symbol type should be the same.

   As an example, someone who wants to use your unification algorithm
   for say Peano arithmetic will define the following structure and
   use it with the `Term` functor given below.

   ```
   structure Peano : SIG where
      datatype symbol = Zero | Succ
	  fun arity Zero = 0
	    | arity Succ = 1
   end
   ```


2. Define a similar signature `VAR` to capture variables. Essentially
   we only need a ORD structure on the variable type but it is good to
   have a separate signature here so that latter on we can add more
   things if required.

3. Define the `Term` functor that takes as arguments two structures
   (S : SIG) and V : VAR and builds a type that captures terms over
   the signature S.symbol and variable set V.var. Write the helper
   functions like `occurs : term * V.t -> bool` that checks whether
   the variable occurs in the term or not.

4. Use the `ORD_MAP` structure to implement an efficient data
   structure for telescopes. We maintain the telescope as a ORD_MAP
   data structure and the problem to solve is to figure out of a new
   equation (xᵢ ≡ tᵢ) can be added to a given map. This finite map
   keeps track of the variable assignment, i.e. the map uses the
   variable as a key and the term associated with it as the value.

   x₁ ≡ t₁, .... xᵢ ≡ tᵢ ... xₙ ≡ tₙ then the map will have n keys
   x₁,....,xₙ with t₁,...,tₙ as the associated values.
