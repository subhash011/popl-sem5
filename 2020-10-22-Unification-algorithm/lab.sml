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
        | occurs (SigTerm (_, xs), v: V.var) = 
            let
                fun occursUtil [] = false
                    | occursUtil (x::xs) = if occurs (x, v) then true else occursUtil(xs)
            in
                occursUtil (xs)
            end

    (* if arity not equal to number of args of function symbol return false *)
    fun isValid (SigTerm (sym, tlist)) = (S.arity sym = List.length (tlist))
        | isValid (VarTerm x) = true

end

functor Telescope (S : SIG) (V: VAR)  =
struct
    structure term = Term (S) (V)

    (* User red black map for managing the telescope *)
    structure telescope = RedBlackMapFn(V.Ord)

    type map = term.term telescope.map

    val empty = telescope.empty

    (* given a list of terms and a variable, check if variable is in the telescope,
        for it to be valid, the term must not be there *)
    fun isValidInsertLs (tele: map) (v: V.var) [] = true
        | isValidInsertLs (tele: map) (v: V.var) (x::xs) = 
            let
                fun isValidInsertTerm (tele: map) (x: V.var) (term.VarTerm v) = 
                    let
                        val search = telescope.find(tele, x)
                    in
                        case search of
                           SOME trm => if term.occurs(trm, v) then false else true
                         | NONE => true
                    end
                    | isValidInsertTerm (tele: map) (x: V.var) (term.SigTerm (sym, ls)) = isValidInsertLs tele v xs

            in
                 if ((isValidInsertTerm tele v x) andalso (isValidInsertLs tele v xs)) then true else false
            end

    (* insert a map into telescope, if it is valid by checking the above cond *)
    fun insert (tele: map) (x: V.var) (term.VarTerm t) : map option =  if V.Ord.compare (x, t) = EQUAL
                                                        then SOME tele
                                                        else let
                                                            val search = telescope.find(tele, x)
                                                        in
                                                            case search of
                                                               SOME trm =>  if term.occurs(trm, t) 
                                                                            then NONE
                                                                            else SOME (telescope.insert(tele, x, term.VarTerm t))
                                                             | NONE => SOME (telescope.insert(tele, x, term.VarTerm t))
                                                        end
        | insert (tele: map) (x: V.var) (term.SigTerm (sym, ls)) : map option = let
                                                            val term = term.SigTerm (sym, ls)
                                                        in
                                                            if term.occurs(term, x) 
                                                            then NONE
                                                            else if (isValidInsertLs tele x ls)
                                                            then SOME (telescope.insert(tele, x, term))
                                                            else NONE
                                                        end
end

functor Unify (S : SIG) (V: VAR) =
struct
    structure telescopes = Telescope (S) (V)

    structure term = telescopes.term

    open telescopes

    open term

    fun lsToPairls [] _ = []
        | lsToPairls _ [] = []
        | lsToPairls (x::xs) (y::ys) = (x, y) :: (lsToPairls xs ys)

    fun unify (tele: map) (SigTerm (sym1, ls1), SigTerm (sym2, ls2)) = if (S.arity sym1 = S.arity sym2) 
                                                                            then unifyList tele (lsToPairls ls1 ls2)
                                                                            else NONE
        | unify (tele: map) (VarTerm v1, VarTerm v2) = let
                                                            val search = (telescope.find (tele, v1), telescope.find (tele, v2)) 
                                                        in
                                                            case search of
                                                               (NONE, NONE) => SOME (telescope.insert (tele, v1, (VarTerm v2)))
                                                               | (SOME x, NONE) => SOME (telescope.insert (tele, v2, x))
                                                               | (NONE, SOME y) => SOME (telescope.insert (tele, v1, y))
                                                               | (SOME x, SOME y) => unify tele (x, y)
                                                        end
        | unify (tele: map) (VarTerm v, SigTerm (sym, ls)) =    let
                                                                    val search = telescope.find (tele, v)
                                                                in
                                                                    case search of
                                                                       NONE => SOME (telescope.insert (tele, v, (SigTerm (sym, ls))))
                                                                     | SOME t => unify tele (t, SigTerm (sym, ls))
                                                                end
        | unify (tele: map) (SigTerm (sym, ls), VarTerm v) = unify tele (VarTerm v, SigTerm (sym, ls))
    
    and unifyList (tele: map) ls = 
        case ls of
        [] => SOME tele
        | x::xs => let 
                        val tele2 = (unify tele x) 
                    in
                        case tele2 of
                        NONE => NONE 
                        | SOME t =>  (unifyList t xs) 
                    end


end

(* Examples section *)

datatype sym = Zero | Succ | Add
datatype var = x | y | z

structure S: SIG =
struct
    type symbol = sym
    fun arity Zero = 0
        | arity Succ = 1
        | arity Add = 2
    structure Ord: ORD_KEY = 
    struct
        type ord_key = sym
        fun compare (a, b) = let
            val aa = arity a
            val ab = arity b
        in
            if aa > ab
            then GREATER
            else if aa < ab
            then LESS
            else EQUAL
        end
    end
end

structure V: VAR =
struct
    type var = var
    structure Ord: ORD_KEY = 
    struct
        type ord_key = var
        fun compare (a, b) = let
            val aa = case a of
               x => 0
             | y => 1
             | z => 2
            val ab = case b of
               x => 0
             | y => 1
             | z => 2
        in
            if aa > ab
            then GREATER
            else if aa < ab
            then LESS
            else EQUAL
        end
    end
end


structure unify = Unify (S) (V)
val tele = unify.empty;
Control.Print.printDepth := 100;
fun App (sym, ls) = unify.SigTerm (sym, ls)
(* 
    Invalid := Succ (x, y) 
    because arity of Succ is 1
*)
val invt1 = App (Succ, [unify.VarTerm x, unify.VarTerm y])
val invt1_ans = unify.isValid invt1

(* 
    Valid: 
        t1 := Add (x, Add (y, z))
        t2 := Add (Succ (y), Add (Zero (z), Succ (x)))
*)

val t1 = App (Add, [unify.VarTerm x, App (Add, [unify.VarTerm y, unify.VarTerm z])])
val t2 = App (Add, [App (Succ, [unify.VarTerm y]), App (Add, [App (Zero, [unify.VarTerm z]), App (Succ, [unify.VarTerm x])])])
val validity = (unify.isValid t1, unify.isValid t2)

val tryUnify = unify.unify tele (t1, t2)

(* 
    from t1 aand t2 we can see that after unification,
    x ≡ Succ (y)
    y ≡ Zero (z)
    z ≡ Succ (x)
*)

val x_t = valOf(unify.telescope.find(valOf(tryUnify), x));
val y_t =  valOf(unify.telescope.find(valOf(tryUnify), y));
val z_t =  valOf(unify.telescope.find(valOf(tryUnify), z));
