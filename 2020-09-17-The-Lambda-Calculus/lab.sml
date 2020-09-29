(*
1. Define a type expr to capture this abstract syntax using ML data types with variables represented as strings
*)

type var = string

datatype expr = VAR of var
        | APPLY of expr * expr
        | FUNC of expr * expr

(*
2. Write a function fresh : string list -> string which will produce a fresh variable name, i.e. given xs : string list, the strin fresh xs will be different from all the strings in xs
*)

fun fresh [] = "a"
    | fresh ls = 
        let
          fun freshutil [] _ = ""
            | freshutil (x::xs) i = 
                let
                    fun getChar x i = 
                        if (i >= 0) andalso (i < (String.size x))
                        then 
                            if String.sub(x, i) = #"a"
                            then "b"
                            else "a"
                        else "a"
                in
                    (getChar x i) ^ (freshutil xs (i+1))
                end
        in
          freshutil ls 0
        end
    
(*
3. Write the function free : expr -> var list to compute the list of all free variables
*)

fun free expr = 
    let
        fun isPresent [] x = false
	        | isPresent (x :: xs) y = if (x = y) then true else isPresent xs y

        fun freeutil ls (VAR(e1)) = if (isPresent ls e1 = true) then [] else [e1]
            | freeutil ls (APPLY(e1, e2)) = freeutil ls e1 @ freeutil ls e2
            | freeutil ls (FUNC(VAR(e1), e2)) = freeutil (e1 :: ls) e2
    in
      freeutil [] expr
    end

(*
4. Write a function subst : var * expr -> expr -> expr where subst (x,N) M substitutes all free occurrence of x in M with N.
*)

fun subst ((VAR x), expr1) (VAR y) = if x = y then expr1 else VAR(y)
    | subst ((VAR x), expr1) (APPLY(e1, e2)) = APPLY ((subst ((VAR x), expr1) e1), (subst ((VAR x), expr1) e2))
    | subst ((VAR x), expr1) (FUNC((VAR e1), e2)) = if e1 = x then FUNC((VAR e1), e2) else FUNC((VAR e1), (subst ((VAR(x)), expr1) e2))


val expr1 = FUNC(VAR("a"), APPLY(VAR("a"), VAR("b")))
val expr2 = FUNC(VAR("a"), APPLY(FUNC(VAR("b"), VAR("b")), VAR("a")))
val vars = ["aba", "baa", "aab"]
val fresh_vars = fresh vars
val free_e1 = free expr1
val free_e2 = free expr2
val subst1 = subst (VAR("y"), expr1) expr2


    