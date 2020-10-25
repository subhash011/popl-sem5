signature SIG =
sig
    type symbol
    val arity: symbol -> int

    (* The where below asserts for the type of ord_key *)

    structure Ord: ORD_KEY where type ord_key = symbol
end

signature VAR = sig
    type var
    structure Ord: ORD_KEY where type ord_key = var
end

functor Term (S : SIG) (V: VAR) =
struct
    (* 
        term: 
            1. VarTerm is a variable
            2. SigTerm is a the function symbol with the list of terms
    *)
    datatype term = VarTerm of V.var | SigTerm of (S.symbol * term list)
    fun occurs (VarTerm tv, v: V.var) = if V.Ord.compare (tv, v) = EQUAL then true else false
        | occurs (SigTerm (_, ls), v: V.var) = 
            let
                fun occursUtil [] = false
                    | occursUtil (x::xs) = if occurs (x, v) then true else occursUtil(xs)
            in
                occursUtil (ls)
            end

    structure telescope = RedBlackMapFn(V.Ord)

end

(* An example for peano datatype *)
datatype peano = Zero | Succ

(* This shows a correct implementation for the Peano arithmetic *)
structure Peano: SIG =
struct
    type symbol = peano
    fun arity Zero = 0
        | arity Succ = 1
    structure Ord: ORD_KEY = 
    struct
        type ord_key = peano
        fun compare (a, b) = EQUAL
    end
end

(*

In the below example, type ord_key is defined as int whereas type symbol is 
defined as peano this throws an error which shows my assertion works

structure Peano: SIG =
struct
    type symbol = peano
    fun arity Zero = 0
        | arity Succ = 1
    structure Ord: ORD_KEY = 
    struct
        type ord_key = int
        fun compare (a, b) = EQUAL
    end
end
*)