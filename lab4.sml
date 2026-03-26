fun flip ([]) = []
    | flip (x::y::xs) = [y]@[x]@flip(xs);

flip([1,2,3,4]);

fun remove ([],_) = []
    | remove (a,1) = tl(a)
    | remove (a,b) = hd(a)::remove(tl(a),b-1);

remove([1,2,3,4],3);

fun square (0) = 0
    | square (n) = square(n-1) +2*n -1;

square(5);

fun flip ([]) = []
    | flip ((a,b)::xs) = if a>b then (b,a)::flip(xs) else (a,b)::flip(xs);

flip([(1,2),(3,1)]);

fun vowel([]) = false
    | vowel (x::xs) = (x = #"a") orelse(x = #"e") orelse(x = #"i") orelse(x = #"o") orelse(x = #"u");

vowel([#"b",#"c"]);

fun member (x,[]) = false
    | member (x,y::L) = if x = y then true else member(x,L);

member(3,[1,2,3]);

fun delete (x,[]) = []
    | delete(x,y::L) = if x = y then L else y::delete(x,L);

delete(3,[1,2,3]);
delete(2,[1,2,3]);

fun insert (x,[]) = [x]
    | insert (x,y::L) = if x = y then y::L else y::insert(x,L);

insert(3,[1,2,3]);
insert(4,[1,2,3]);

(*fun insertAll(a,[[]]) = [[a]]
    | insertAll (a,x::xs) = (a::x)::insertAll(a,xs);*)

fun prodDiff (nil) = 1
    | prodDiff (x::xs) = prodAusil(x,xs)*prodDiff(xs)
and prodAusil (x,[]) = 1
    | prodAusil (x,y::l) = (x-y)*prodAusil(x,l);

prodDiff([1,2,3]);

fun is_one (1) = "one"
    | is_one(_) = "anything else";

is_one(12);

fun powerset [] = [[]]
  | powerset (x::xs) = powerset xs @ map (fn ys => x::ys) (powerset xs);

powerset([1,2,3]);