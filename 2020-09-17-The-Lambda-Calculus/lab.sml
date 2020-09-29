(*
Define a type expr to capture this abstract syntax using ML data types with variables represented as strings
*)


(*
Write a function fresh : string list -> string which will produce a fresh variable name, i.e. given xs : string list, the strin fresh xs will be different from all the strings in xs
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