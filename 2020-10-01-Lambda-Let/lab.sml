(* 1 *)

type var = string;

datatype expr =  
    VAR of var
    | APPLY of expr*expr
    | LAMBDA of var*expr

datatype lambdalet = 
    VAR_LET of var
    | LAMBDA_LET of var*lambdalet
    | APPLY_LET of lambdalet*lambdalet
    | LET of var*lambdalet*lambdalet


datatype lambdaletrec = 
    VAR_LET_REC of var
    | LAMBDA_LET_REC of var*lambdaletrec
    | APPLY_LET_REC of lambdaletrec*lambdaletrec
    | LET_REC of var*lambdaletrec*lambdaletrec

(* 2 *)


fun unlet (VAR_LET x) = VAR (x)
    | unlet (LAMBDA_LET (e1, e2)) = LAMBDA (e1, unlet e2)
    | unlet (APPLY_LET(e1, e2)) = APPLY(unlet e1, unlet e2)
    | unlet (LET (e1, e2, e3)) = APPLY (LAMBDA(e1, unlet e3), unlet e2)

fun unletrec (VAR_LET_REC x) = VAR_LET (x)
    | unletrec (LAMBDA_LET_REC (e1, e2)) = LAMBDA_LET (e1, unletrec e2)
    | unletrec (APPLY_LET_REC (e1, e2)) = APPLY_LET(unletrec e1, unletrec e2)
    | unletrec (LET_REC (e1, e2, e3)) = 
        (* 
            Y = λf.(λx.f(x x))(λx.f(x x)) 
            The function Y computes the fixed point of LAMBDA_LET_REC
        *)
        let
            fun Y (LAMBDA_LET_REC (e1, e2)) =
            let
                val f = LAMBDA_LET_REC (e1, e2)
            in
                APPLY_LET_REC 
                ( 
                    LAMBDA_LET_REC ( "x", APPLY_LET_REC ( f, APPLY_LET_REC ( VAR_LET_REC ("x"), VAR_LET_REC ("x") ) ) ),
                    LAMBDA_LET_REC ( "x", APPLY_LET_REC ( f, APPLY_LET_REC ( VAR_LET_REC ("x"), VAR_LET_REC ("x") ) ) )
                ) 
            end
            | Y f = f
        in
          LET (e1, unletrec (Y (LAMBDA_LET_REC(e1, e2))), unletrec e3)
        end



(* Examples *)

(* Example for unlet to get a lambda expression *)

(* 
    Just a normal let - in application
    Not an exact defination because '+' is not implemented so
    this is just for testing purpose
*)
val ex1 = LET ("x", VAR_LET("6"), VAR_LET("x + 5"))
val unletex1 = unlet ex1

(* 
    Just a random recursive function
    letrec x = x(n-1) in fact 10 
*)

val ex2 = LET_REC ( "fact", LAMBDA_LET_REC ("n", APPLY_LET_REC ( VAR_LET_REC ("fact"),  VAR_LET_REC ("n - 1"))) , APPLY_LET_REC ( VAR_LET_REC("fact"), VAR_LET_REC ("10") ) )
val unletrecex2 = unletrec ex2;

(* This time unlet after unletrec so we get lambda expr from letrec *)
val unletex2 = unlet (unletrec ex2)