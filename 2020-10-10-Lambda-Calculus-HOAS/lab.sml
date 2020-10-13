datatype lam = V of string
    | A of lam * lam
    | L of string * lam

datatype hlam = HV of string
    | HA of hlam * hlam
    | HL of hlam -> hlam

(* 1 *)

fun subst (x: string, u: hlam) (HV e) = if e = x then u else HV e
    | subst (x: string, u: hlam) (HA (e1, e2)) = HA (subst (x, u) e1, subst (x, u) e2)
    | subst (x: string, u: hlam) (HL f) = 
                let
                  fun fp t = subst (x, u) (f t)
                in
                  HL fp
                end

(* 2 *)

fun abstract (x: string) (mh: hlam) = 
    let
      fun f (nh: hlam) = subst (x, nh) mh
    in
      HL f
    end

(* 3 *)

fun freshen (e: hlam) = 
    let
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
        fun freeP (HV e1) = [e1]
            | freeP (HA (e1, e2)) = (freeP e1) @ (freeP e2)
            | freeP (HL f) = freeP (f (HV "x"))
    in
      fresh (freeP e)
    end

(* 4 *)

fun hoas (V e1) = HV e1
    | hoas (A (e1, e2)) = HA (hoas e1, hoas e2)
    | hoas (L (x, e1)) = abstract x (hoas e1)

fun syntax (HV e1) = V e1
    | syntax (HA (e1, e2)) = A (syntax e1, syntax e2)
    | syntax (HL f) = 
        let
          val cookvar = freshen (HL f)
        in
          L (cookvar, syntax (f (HV cookvar)))
        end

(* Auxilary function for testing purpose *)
fun apply (HL f) x = f x
    | apply (HA (e1, e2)) _ = apply e1 e2
    | apply (HV e) x = x

(* Examples section *)

val identity = L ("t", V "t")
val identityH = HL (fn t => t)

(* e1 = fn t => t *)
val e1 = HL (fn t => HA (t, HV "x"))
(*
The below subst gives 
subste1 = HL (fn t => HA (t, HV "y"))
so apply subste1 (HV "aa") should give HA ("aa", "y")
*)
val subste1 = subst ("x", HV "y") e1
val fsubste1 = apply subste1 (HV "aa") (*try HV "x" instead*)
(*
The below abstract gives 
abste1 = HL (fn x => HL (fn t => HA (t, x)))
1. apply abste1 (HV "aa") should give HL (fn t => HA (t, HV "aa")) --> temp
2. apply temp (HV "ab") should give HA (HV "ab", HV "aa")
*)
val abste1 = abstract "x" e1
val fabste1 = apply (apply abste1 (HV "aa")) (HV "ab")

(* e2 = fn x => fn t => (fn y => y) t *)
val e2 = HL (fn x => HL (fn t => HA (identityH, t)))
val applye2 = apply (apply (apply e2 (HV "aa")) (HV "ab")) (HV "ac")
val freshe2 = freshen e2

(* e3 = fn x => fn t => (fn t => t) t *)
(* hoas e3 shoud give e2 *)
val e3 = L ("x", L ("t", A (identity, V "t")))
val hoase2 = hoas e3;
val applyhoase2 = apply (apply (apply e2 (HV "aa")) (HV "ab")) (HV "ac")