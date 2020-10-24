# Lists and Trees

Deadline : 2nd Sep 2020, 23:59 hrs

In this assignment we look at functions on lists and binary trees.

1. For lists define the function `map : ('a -> 'b) -> 'a list -> 'b
   list`. The semantics of map is that it applies the given function
   on all the elements of the list, i.e.

        map f [x1, x2,....,xn] = [f x1, f x2, ... f xn]

2. Define the data type `'a tree` that captures a binary tree.

3. Can you write a function `treemap` analogues to `map` for list ?
   First write its type and then complete its definition.

4. Define the in-order, pre-order and post-order traversal of the
   binary tree returning the list of nodes in the given order. First
   write down the type of the function(s) and then go about defining
   them.

5. Define the rotate clockwise function for binary trees. Pictorially
   this rotation function is defined as the following.



                   a                   b
                  / \                 / \
                 / ⭮ \               /   \
                b    🌲₃  ======>   🌲₁   a
               / \                       / \
              /   \                     /   \
             🌲₁   🌲₂                 🌲₂    🌲₃

	If the left subtree of the root is null then rotation is identity operation.
