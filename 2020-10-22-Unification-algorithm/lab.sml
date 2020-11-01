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
    fun isValid (SigTerm (sym, tlist)) = 
            let
                fun isValidUtil [] = true
                    | isValidUtil (x::xs) = (isValid x) andalso (isValidUtil xs)
            in
                (S.arity sym = List.length(tlist)) andalso (isValidUtil tlist)
            end
        | isValid (VarTerm x) = true

end

functor Telescope (S : SIG) (V: VAR)  =
struct
    structure term = Term (S) (V)

    (* User red black map for managing the telescope *)
    structure telescope = RedBlackMapFn(V.Ord)

    type map = term.term telescope.map

    val empty = telescope.empty

    fun isValidInsertTerm (tele: map) (x: V.var) (term.VarTerm v) = 
                    let
                        val search = telescope.find(tele, x)
                    in
                        case search of
                           SOME trm => if term.occurs(trm, v) then false else true
                         | NONE => true
                    end
        | isValidInsertTerm (tele: map) (x: V.var) (term.SigTerm (sym, ls)) =
            (S.arity sym = List.length(ls)) andalso (isValidInsertLs tele x ls)

    and isValidInsertLs (tele: map) (v: V.var) [] = true
        | isValidInsertLs (tele: map) (v: V.var) (x::xs) = 
                 if ((isValidInsertTerm tele v x) andalso (isValidInsertLs tele v xs)) then true else false

    (* insert a map into telescope, if it is valid by checking the above cond *)
    fun insert (tele: map) (x: V.var) (term.VarTerm t) : map option =  if V.Ord.compare (x, t) = EQUAL
                                                        then SOME tele
                                                        else let
                                                            val search = telescope.find(tele, x)
                                                        in
                                                            case search of
                                                               SOME trm =>  if term.occurs(trm, t) 
                                                                            then NONE
                                                                            else if (isValidInsertTerm tele x (term.VarTerm t))
                                                                            then SOME (telescope.insert(tele, x, term.VarTerm t))
                                                                            else NONE
                                                             | NONE =>  if (isValidInsertTerm tele x (term.VarTerm t))
                                                                        then SOME (telescope.insert(tele, x, term.VarTerm t))
                                                                        else NONE
                                                        end
        | insert (tele: map) (x: V.var) (term.SigTerm (sym, ls)) : map option = let
                                                            val t = term.SigTerm (sym, ls)
                                                        in
                                                            if term.occurs(t, x) 
                                                            then NONE
                                                            else if (isValidInsertLs tele x ls)
                                                            then SOME (telescope.insert(tele, x, t))
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

    fun unify (tele: map) (SigTerm (sym1, ls1), SigTerm (sym2, ls2)) : map option = if (S.arity sym1 = S.arity sym2) 
                                                                        andalso (S.arity sym1 = List.length (ls1)) 
                                                                        andalso (S.arity sym2 = List.length (ls2))
                                                                        then unifyList tele (lsToPairls ls1 ls2)
                                                                        else NONE
        | unify (tele: map) (VarTerm v1, VarTerm v2) : map option = let
                                                            val search = (telescope.find (tele, v1), telescope.find (tele, v2)) 
                                                        in
                                                            case search of
                                                               (NONE, NONE) => (insert tele v1 (VarTerm v2))
                                                               | (SOME x, NONE) => (insert tele v2 x)
                                                               | (NONE, SOME y) => (insert tele v1 y)
                                                               | (SOME x, SOME y) => (unify tele (x, y))
                                                        end
        | unify (tele: map) (VarTerm v, SigTerm (sym, ls)) : map option =    let
                                                                    val search = telescope.find (tele, v)
                                                                in
                                                                    case search of
                                                                       NONE => (insert tele v (SigTerm (sym, ls)))
                                                                     | SOME t => (unify tele (t, SigTerm (sym, ls)))
                                                                end
        | unify (tele: map) (SigTerm (sym, ls), VarTerm v) : map option = (unify tele (VarTerm v, SigTerm (sym, ls)))
    
    and unifyList (tele: map) ls : map option = 
        case ls of
        [] => SOME tele
        | x::xs => let 
                        val tele2 = (unify tele x) 
                    in
                        case tele2 of
                        NONE => NONE
                        | SOME t => (unifyList t xs) 
                    end


end

(* Examples section *)

(* 
    The structures are similar to the peano type
    but an addional type 'Add' was added to have arity 2
    for testing
*)

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

structure unify = Unify (S) (V);
fun App (sym, ls) = unify.SigTerm (sym, ls)
fun Var v = unify.VarTerm v
val X = Var x
val Y = Var y
val Z = Var z;
Control.Print.printDepth := 100;
fun add ls = App (Add, ls)
fun succ ls = App (Succ, ls)
fun zero ls = App (Zero, ls)

(* 
    Invalid := Succ (x, y) 
    because arity of Succ is 1
*)
val invt1 = App (Succ, [X, Y])
val invt1_ans = unify.isValid invt1

(* 
    Valid: 
        t1 := Add (x, Add (y, z))
        t2 := Add (Succ (y), Add (Zero, Succ (x)))
        t3 := Add (x, y)
        t4 := Add (Add (Succ (Zero), Succ (y)), Add (Zero, Succ (z)))
*)

val t1 = add [X, add [Y, Z]]
val t2 = add [succ [Y], add [zero [], succ [X]]]
val t3 = add [X, Y]
val t4 = add [add [succ [zero []], succ [Y]], add [zero [], succ [Z]]]
val validity = (unify.isValid t1, unify.isValid t2)

val tele1 = unify.empty;
val ut1t2 = unify.unify tele1 (t1, t2)

val tele2 = unify.empty;
val ut3t4 = unify.unify tele2 (t3, t4)

(* 
    from t1 aand t2 we can see that after unification,
    x ≡ Succ (y)
    y ≡ Zero
    z ≡ Succ (x)

    from t3 and t4 we can see that
    x ≡ Add (Succ (Zero), Succ (y))
    y ≡ Add (Zero, Succ (z))
*)

fun showVal a b = 
    case a of
       NONE => NONE
     | SOME ax => SOME (valOf(unify.telescope.find(ax, b)))

val x_t12 = showVal ut1t2 x;
val y_t12 = showVal ut1t2 y;
val z_t12 = showVal ut1t2 z;

val x_t34 = showVal ut3t4 x;
val y_t34 = showVal ut3t4 y;


(* 
    invalid:
        a1 := Add (x, Add (y, z))
        a2 := Add (Succ (y), Add (Zero (y), Succ (x)))
        a2 is invalid because arity of Zero is 0 but it has one arg here
        b1 := Add (x, y)
        b2 = Add (Succ (x), z)
        unifying both is invalid because x and Succ (x) leads to recursion
    so unifying a1 and a2 will give NONE

*)
val tele3 = unify.empty
val tele4 = unify.empty

val a1 = add [X, add [Y, Z]]
val a2 = add [succ [Y], add [zero [Y], succ [X]]]
val b1 = add [X, Y]
val b2 = add [succ [X], Z]

val isValida2 = unify.isValid a2;
val unifya12 = unify.unify tele3 (a1, a2)
val a3 = showVal unifya12 x
val unifyb12 = unify.unify tele4 (b1, b2)
val b3 = showVal unifyb12 x