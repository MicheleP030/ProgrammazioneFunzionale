exception parametri

fun curry (F,x,y,z) = F((x,y,z))
    | curry (_) = raise parametri;

curry ((fn (x,y,z)=>x*y*z),1,2,3);

(*foldl e foldr prendono come argomenti la funzione, l accumulatore e la lista credo*)

fun Reals (F,l) = map F l
    | Reals (_) = raise parametri;

fun RealsFoldr xs =
    foldr (fn (x, acc) => Real.fromInt x :: acc) [] xs;

fun RealsFoldl xs =
    foldl (fn (x, acc) => Real.fromInt x :: acc) [] xs;

Reals(Real.fromInt,[1,23,4]);
RealsFoldl([1,23,4]);
RealsFoldr([1,23,4]);

fun ands (y::l) =
    foldr (fn (x, acc) => x andalso acc) y l
  | ands [] = true;

fun implode (l) = 
    foldr (fn (x,acc) => str(x)^acc) "" l;

datatype 'a btree = 
    Empty | 
    Noda of 'a * 'a btree * 'a btree;

fun postOrder (Empty) = []
    | postOrder (Noda(vals,left,right)) = postOrder(left) @ postOrder(right) @ [vals];

fun inOrder (Empty) = []
    | inOrder (Noda(vals,left,right)) = inOrder(left) @ [vals] @ inOrder(right);

implode([#"a",#"b"]);
ands([true,true,false]);
(*
(*Nodo è il nome del costruttore che chiamo quando creo il tupltree*)
datatype ('a,'b) tupltree = 
    Empty
    | Nodo of ('a*'b) * ('a,'b) tupltree * ('a,'b) tupltree;

val t1 = Nodo((#"a",1),Empty,Empty)

fun sumTree(Empty) = 0
    | sumTree (Nodo((a,vals),left,right)) = sumTree(left)+sumTree(right)+vals;

sumTree(t1);
*)
(*escludendo le foglie, se la funz passata ritorna true aggingo 1*)

fun countInternalTree(f,Empty) = 0
    | countInternalTree(f,Noda(a,Empty,Empty)) = 0
    | countInternalTree(f,Noda(a,left,right)) = if f(a) then 1+countInternalTree(f,left)+countInternalTree(f,right) else countInternalTree(f,left)+countInternalTree(f,right);

datatype 'a bltree = Empty
| Leaf of 'a
| Node of 'a * 'a bltree * 'a bltree;

fun doubleTree(Empty) = Empty
    | doubleTree (Leaf(a)) = Leaf(a*2)
    | doubleTree (Node(a,left,right)) = Node(a*2,doubleTree(left),doubleTree(right));