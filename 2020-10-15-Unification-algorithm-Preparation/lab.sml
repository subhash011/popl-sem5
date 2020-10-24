signature SIG =
sig
    type symbol
    val findArity: symbol -> int
    structure Ord: ORD_KEY;
end
		    
signature VAR = sig
    structure Ord: ORD_KEY
end