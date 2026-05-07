datatype ('label) tree =
    Node of 'label * 'label tree list;

exception Missing;

signature Simple = sig
    type simpleTree = int tree
    val build: int->(simpleTree list) -> simpleTree
    val subTree: int*simpleTree -> simpleTree
end;

structure SimpleTree: Simple = struct
    type simpleTree = int tree
    fun build a L = Node(a,L)
    fun subTree(i,(Node(a,nil))) = raise Missing
        | subTree (i,(Node(a,L))) = if i < 0 then raise Missing else if i = 0 then hd(L) else subTree (i-1,Node(a,tl(L)))
end;

structure Tree = struct 
    type 'a t = 'a tree
    fun create a = (Node(a,nil))
    fun build a L = Node(a,L)
    fun subTree(i,(Node(a,nil))) = raise Missing
        | subTree (i,(Node(a,L))) = if i < 0 then raise Missing else if i = 0 then hd(L) else subTree (i-1,Node(a,tl(L)))
end;

exception EmptyStack;

structure Stack = struct 
    type 'a stack = 'a list
    fun create () = []
    fun push l s = l::s
    fun pop [] = raise EmptyStack
        | pop s = tl s
    fun isEmpty s = s = []
    fun top [] = raise EmptyStack
        | top s = hd(s)
end;

signature StringStack = sig
    type stringStack = string list
    val create: stringStack
    val push: string -> stringStack -> stringStack
    val pop: stringStack -> stringStack
    val isEmpty: stringStack -> bool
end; 



fun isOn a (Node (b,t)) = if a = b then true else foldr (fn(x,y) => x orelse y ) false (map (isOn a) t);

isOn 3 (Node(2, [Node (3,nil), Node(5,nil)])) = true;

fun count a (Node (b,t)) = if a = b then 1+foldr (fn(x,y) => x + y ) 0 (map (count a) t) else foldr (fn(x,y) => x + y ) 0 (map (count a) t);

count 2 (Node(2, [Node (3,nil), Node(2,nil)])) = 2;

fun depth (Node (b,nil)) = 1
    | depth (Node (b,t)) = 1 + foldr Int.max 0 (map (depth) t);

depth (Node(2, [Node (3, [Node(4,nil)]), Node(2,nil)])) = 3;

fun preOrder (Node (b, t)) = b :: foldr (fn (t, acc) => preOrder t @ acc) [] t;

preOrder (Node(2, [Node (3, [Node(4,nil)]), Node(2,nil)]));