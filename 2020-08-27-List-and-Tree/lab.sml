(*
1. For lists define the function map : ('a -> 'b) -> 'a list -> 'b list
*)

fun map _ [] = []
    | map f (x::xs) = (f x)::(map f xs)

(*
2. Define the data type 'a tree that captures a binary tree.
*)

datatype 'a tree = null
        | node of ('a tree * 'a * 'a tree);


val atree = node(node(null, 1, null), 3, node(null, 4, null));
val btree = node(node(node(null, 1, null), 3, node(null, 4, null)), 6, node(null, 8, null));

(*
3. Can you write a function treemap analogues to map for list ? First write its type and then complete its definition.
*)

(*
type: fn: ('a - > 'b) -> 'a tree -> 'b tree
*)
fun treemap _ null = null
    | treemap f (node (left, data, right)) = node (treemap f left, (f data),treemap f right);


(*
4. Define the in-order, pre-order and post-order traversal of the binary tree returning the list of nodes in the given order. First write down the type of the function(s) and then go about defining them.
*)

(*
    type: fn: 'a tree = 'a list
*)

fun preorder null = []
    | preorder (node (left, data, right)) = [data] @ (preorder left) @ (preorder right);

fun inorder null = []
    | inorder (node (left, data, right)) = (inorder left) @ [data] @  (inorder right);

fun postorder null = []
    | postorder (node (left, data, right)) = (postorder left) @ (postorder right) @ [data];


(*
5. Define the rotate clockwise function for binary trees.
*)

fun rotate null = null
    | rotate (node (null, data, right)) = node (null, data, right)
    | rotate (node (node(lleft, ldata, lright), data, right)) = node(lleft, ldata, node(lright, data, right));
        

(*
other functions on trees
*)

fun height null = 0
    | height (node (left, data, right)) = 
        let
          fun max a b = if a > b then a else b
        in
          (max (height left) (height right)) + 1
        end


fun depth _ null _ = ~1 |
    depth null _ _ = ~1 |
    depth (node (tleft, tdata, tright)) (node (left, data, right)) value = 
    let
        fun max a b = if a > b then a else b
        fun equals a b = 
    in
        if tdata = data
        then value + 1
        else max (depth (node (tleft, tdata, tright)) left (value + 1)) (depth (node (tleft, tdata, tright)) right (value + 1))
    end