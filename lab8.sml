signature SETs = sig
  type 'a set
  val emptyset : 'a set
  val addin : 'a * 'a set -> 'a set
  val isin : 'a * 'a set -> bool
  val removefrom : 'a * 'a set -> 'a set
end

signature SETs = sig
    type 'a set
end;

structure ListSet : SETs = struct
    type 'a set = 'a list
end

val s : int ListSet.set = [1,2,3]

signature SET2 = sig
    type 'a set
    val emptyset: 'a set
    val isin : ''a -> ''a set -> bool
    val addin: ''a -> ''a set -> ''a set
    val removefrom: ''a -> ''a set -> ''a set
end;

structure Set2 = struct
    type 'a set = 'a list
    val emptyset = []
    fun isin a []  = false
        | isin a (x::l) = if a = x then true else isin a l 
    fun addin a l = if isin a l then l else a::l
    fun removefrom a [] = []
        | removefrom a (x::l) = if a = x then l else x::removefrom a l 
end; 



signature ALBERO = sig
    datatype 'a tree = Lf | Br of 'a * 'a tree * 'a tree
    val countNodes : 'a tree -> int
    val depth : 'a tree -> int
    val mirror : 'a tree -> 'a tree
end;

structure Albero = struct
    datatype 'a tree = Lf | Br of 'a * 'a tree * 'a tree   
    fun countNodes Lf = 0
        | countNodes (Br (x,y,z)) = 1+countNodes(y)+countNodes(z) 
    fun depth Lf = 0
        | depth (Br (_, y, z)) =
            let
                val dy = depth y
                val dz = depth z
            in
                if dy > dz then 1 + dy else 1 + dz
            end
    fun mirror Lf = Lf
        | mirror (Br (z,x,y)) = (Br(z,mirror(y),mirror(x)))
end;

val t : int Albero.tree = Albero.Br (3, Albero.Br (2, Albero.Lf, Albero.Lf), Albero.Br (5, Albero.Br (4, Albero.Lf, Albero.Lf), Albero.Lf));
val count = Albero.countNodes(t);
val depth = Albero.depth(t);
val mirror = Albero.mirror(t);


datatype 'a btree = Empty
|Node of 'a * 'a btree * 'a btree;

exception missing;

fun lookup lt Empty = raise missing 
    | lookup x (Node ((a,b),c,d)) = if a = x then b else if a<x then lookup x c else lookup x d

