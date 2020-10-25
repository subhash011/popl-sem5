signature SIG =
sig
    type symbol
    val arity: symbol -> int
    structure Ord: ORD_KEY where type ord_key = symbol
end

signature VAR = sig
    type var
    structure Ord: ORD_KEY where type ord_key = var
end

functor Term (S : SIG) (V: VAR) =
struct
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