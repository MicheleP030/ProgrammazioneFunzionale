fun fact x = 
    if x = 1 then 1 else x*fact(x-1); 

fact(5);

fun cyclei (l,i) =
    if i = 0 then l else cyclei(tl(l)@[hd(l)],i-1);

cyclei([1,2,3,4],2);

fun duplicate (l) = 
    if l = [] then [] else [hd(l)]@[hd(l)]@duplicate(tl(l));

duplicate([1,2]);

fun len(l) = 
    if l = [] then 0 else 1+len(tl(l));

len([1,2,3]);

fun pow (x,i) = 
    if i = 0 then 1.0 else x*pow(x,i-1);

pow(3.2,4);

fun maxList (l) = 
    if l = [] then "" else if hd(l) > maxList(tl(l)) then hd(l) else maxList(tl(l));

maxList(["ab","a","abc"]);

fun patternpow 1 n = 1
    | patternpow n 1 = n
    | patternpow n m = n*patternpow n m-1;

fun patterncycle ([],i) = []
    | patterncycle (l,0) = l
    | patterncycle (l,i) = patterncycle(tl(l)@[hd(l)],i-1) ;

patterncycle([1,2,3],2);

fun patternduplicate ([]) = []
    | patternduplicate (l) = [hd(l)] @ [hd(l)] @ patternduplicate(tl(l));

patternduplicate ([1,2,3]);

fun patternmaxlist [] = ""
    | patternmaxlist l = if hd(l) > maxList(tl(l)) then hd(l) else maxList(tl(l));

patternmaxlist(["ci","aos","a"]);