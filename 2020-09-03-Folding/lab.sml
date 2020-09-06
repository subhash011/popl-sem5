(*the below lines are for testing my code*)
val a = [1, 2, 3, 4];

fun pow a = a * a;

fun cond x = (x mod 2) = 0;

(*Assignment starts here*)
(*
1. Define foldl and foldr
*)
fun foldr _ y [] = y
    | foldr f y (x :: xs) = f (x, (foldr f y xs))

fun foldl _ y [] = y
    | foldl f y (x::xs) = 
        foldl f (f (x, y)) xs

(*
2. define sum of int list
*)
fun sum lst = 
    let
        fun add (a, b) = a + b;
    in
        foldl add 0 lst
    end

(*
3. partittion function with split function which decides in which list to add the current element
*)
fun partition cond lst = 
    let
        fun split (a, (pos, neg)) = if (cond a) then (a :: pos, neg) else (pos, a :: neg);
    in
        foldr split ([], []) lst
    end

(*
4. map = fn: ('a -> 'b) -> 'a list -> 'b list
    f = fn: 'a -> 'b
*)
fun map _ [] = []
    | map f lst = 
        let
            fun append (a, b) = (f a) :: b
        in
            foldr append [] lst
        end

(*
5. reverse = fn: 'a list -> 'a list
   revappend = fn: 'a * 'a list -> 'a list 
*)
fun reverse lst = 
    let
        fun revappend (a, b) = a :: b;
    in
        foldl revappend [] lst
    end


datatype 'a Find = NotFound of int | Found of 'a;

(*
6. nth = fn: 'a list * int -> 'a option
   isith = fn: 'a * ('a * int * bool) -> ('a * int * bool)
*)
fun nth lst i = 
    let
        fun isith (curel, (NotFound n)) = 
                if n = 0
                then Found curel
                else NotFound (n - 1)
                | isith (_, (Found x)) = Found x

    in
        foldl isith (NotFound i) lst
    end