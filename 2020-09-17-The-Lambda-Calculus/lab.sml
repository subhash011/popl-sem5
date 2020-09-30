(* Q1 *)

type var = string

datatype expr = VAR of var
        | APPLY of expr * expr
        | FUNC of expr * expr

(* Q2 *)

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
    
(* Q3 *)

fun free expr = 
    let
        fun isPresent [] x = false
	        | isPresent (x :: xs) y = if (x = y) then true else isPresent xs y

        fun freeutil ls (VAR e1) = if (isPresent ls e1 = true) then [] else [e1]
            | freeutil ls (APPLY(e1, e2)) = freeutil ls e1 @ freeutil ls e2
            | freeutil ls (FUNC(VAR e1, e2)) = freeutil (e1 :: ls) e2
    in
      freeutil [] expr
    end

(* Q4 *)

fun subst ((VAR x), expr1) (VAR y) = if x = y then expr1 else VAR(y)
    | subst ((VAR x), expr1) (APPLY(e1, e2)) = APPLY ((subst ((VAR x), expr1) e1), (subst ((VAR x), expr1) e2))
    | subst ((VAR x), expr1) (FUNC((VAR e1), e2)) = if e1 = x then FUNC((VAR e1), e2) else FUNC((VAR e1), (subst ((VAR x), expr1) e2))

(* examples *)

val expr1 = FUNC(VAR("a"), APPLY(VAR("a"), VAR("b")))
val expr2 = FUNC(VAR("a"), APPLY(FUNC(VAR "b", VAR "b"), VAR "a"))
val vars = ["aba", "baa", "aab"]
val fresh_vars = fresh vars (*gives a fresh variable*)
val free_e1 = free expr1 (*gives all free vars in expr1*)
val free_e2 = free expr2
val subst1 = subst (VAR("y"), expr1) expr2 (*performs subsitution*)