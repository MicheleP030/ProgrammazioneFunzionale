fun is_lower_than_5 n =
    case n < 5 of
        true => 1
      | false => 2;

       
is_lower_than_5(4);
is_lower_than_5(6);

fun thousandth_power 0 = 0
    | thousandth_power 1 = 1
    | thousandth_power n = 
        let 
            val dieci = n*n*n*n*n*n*n*n*n*n
            val mille = dieci*dieci*dieci
        in
            mille
        end;
 
 thousandth_power(2);

fun split(nil) = (nil,nil)
    | split([a]) = ([a],nil)
    | split (a::b::cs) =
        let
            val K = split (cs);
            val M = #1 K;
            val N = #2 K;
        in
            (a::M,b::N)
        end;

split([1,2,3,4,5]);
(*
fun powerSet(nil) = [nil]
    | powerSet(x::xs) =
    let
        val power = powerSet(xs)
        val sigma = insertAll(x,power(xs))
    in 
        power@sigma
    end;
*)
fun sumPairs([]) = (0,0)
    | sumPairs ((x,y)::xs) = 
        let 
            val (w,z) = sumPairs(xs);
        in 
            (x+w,y+z)
        end;

sumPairs([(1,2),(2,3),(3,4)]);
    
fun maxList([x:real]) = x
    | maxList(x::xs) =
        let
            val max = maxList(xs);
            val res = if x > max then x else max;
        in 
            res
        end;

maxList([5.0,6.7,1.0,9.0,8.0,6.0]);

fun doubleExp (x:real,0) = x 
    |doubleExp (x,i) = 
        let 
            val b = doubleExp(x,i-1);
        in
            b*b
        end;

doubleExp(1.1,3);

fun sumList ([]) = (0,0)
    | sumList (x::y::xs) = 
        let 
            val (pari,dispari) = sumList(xs);
        in
                (x+pari,y+dispari)
        end
    | sumList([x]) = (x,0);

sumList([1,2,3,4,5,6,7]);

fun printList ([]) = print("")
    | printList (x::xs) = (print(Int.toString(x)^"\t"); printList(xs));

printList([1,2,344]);

fun fact 0 = 1
    | fact n = n*fact(n-1);

fun com (n:int,m:int) = print(Int.toString(fact(n)div(fact(m)*fact(n-m)))); 

com(5,4);

fun printXs(1) = "X"
    | printXs n = "X"^printXs(n-1)^printXs(n-1);

printXs(3);