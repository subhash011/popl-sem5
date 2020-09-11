signature COUNTER =
sig
    val incr : unit -> unit
    val decr : unit -> unit
    val get : unit -> int
end

structure Counter : COUNTER =
struct
    val cell : int ref = ref 0
    fun incr () = cell := (!cell + 1)
    fun decr () = cell := (!cell - 1)
    fun get () = !cell
end

functor MkCounter () : COUNTER =
struct
    val cell : int ref = ref 0
    fun incr () = cell := (!cell + 1)
    fun decr () = cell := (!cell - 1)
    fun get () = !cell
end