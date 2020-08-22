(*
val _ = use("2020-08-20-Basic-functions/lab.sml");
*)

(*
1. Write the tri-variate (i.e. 3 variable) versions of curry and uncurry functions. First step is to write down the type (in the comments).
*)

(*
type:  ('a * 'b * 'c -> 'd) -> 'a -> 'b -> 'c -> 'd
*)
fun curry f x y z = f(x, y, z);
(*
type: ('a -> 'b -> 'c -> 'd) -> 'a * 'b * 'c -> 'd
*)
fun uncurry f (x, y, z) = f x y z;


(*
2. Write the functions fst : 'a * 'b -> 'a and snd : 'a * 'b -> 'b that project a tuple into its components.
*)

fun fst (x, _) = x;
fun snd (_, y) = y;

(*
3. Write the length function for lists length : 'a list -> int.
*)

fun len [] = 0 | len (x::xs) = 1 + len xs;

(*
4. Write the reverse : 'a list -> 'a list function. Be careful to not make it O(n^2)
*)


fun revhelp [] x = x
    | revhelp (x::xs) y = revhelp xs (x::y);

fun reverse [] = []
    | reverse x = revhelp x [];

(*
5. Write a function to compute the nth element in the Fibonacci sequence fib : int -> int. Be careful for the obvious version is exponential.
*)


fun fibiter lst (prevval: IntInf.int) (curval: IntInf.int) iter =
    if iter = lst
    then curval
    else fibiter lst (prevval + curval) prevval (iter + 1);

fun fib n = fibiter n 0 1 0;