type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a tree

val your_tree: int tree
val is_perfect: 'a tree -> bool
val tree_max: 'a tree -> 'a 
val lookup: 'a -> 'a tree -> bool
val inorder: 'a tree -> 'a list
val insert: 'a -> 'a tree -> 'a tree
val tree_of_list: 'a list -> 'a tree
val delete: 'a -> 'a tree -> 'a tree

