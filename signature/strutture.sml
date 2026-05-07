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

(*se voglio creare una signature specifica faccio una structure generica, poi la uso nella signature specifica e poi implemento la structure specifica*)