val infile = TextIO.openIn("file.txt");(*apro il file*)
TextIO.inputN(infile,5); (* 5 lettere *)
TextIO.inputLine(infile); (*una riga*)
TextIO.lookahead(infile); (*primo carattere senza consumarlo*)
TextIO.input(infile); (*il resto del file*) 
TextIO.closeIn(infile); (*chiudo files*)


fun getWord(infile) =  
    let 
        val txt = TextIO.inputN(infile,1)
    in

        if txt = " " orelse TextIO.endOfStream(infile)
        then 
            ""
        else 
            txt^getWord(infile)
    end;

fun getList(infile) = 
        if TextIO.endOfStream(infile) 
            then 
                []
            else  
                getWord(infile)::getList(infile);

fun startedList() = 
    let 
        val infile = TextIO.openIn("lista.txt")
    in 
        getList(infile)
    end;

startedList();

exception shortList of int;
dAndSualsiasi n*)

exception negativo of int

fun fact(0) = 0
    | fact (1) = 1
    | fact (n) = if n>0 then n*fact(n-1) else raise negativo n;

val y = fact(~5) handle negativo n => n; (*y deve essere di tipo int quindi non posso mettere il print qua*)
print(Int.toString(y));

(*se voglio che all errore della funzione succeda qualcosa come un print allora posso usare unit come tipo di ritorno dell errore*)
(*exception errore of unit*int => raise errore (print("ciao"),0)*)

fun tabulate (a,k,n,i,F) = 
    if(i=n) thendAndS);

exception EmptyList;

fun reduce (F,nil) = raise EmptyList
| reduce (F,[a]) = a
| reduce (F,x::xs) = F(x, reduce(F,xs));

reduce((fn(x,y) => if x > y then x else y),[1.1,2.2,3.3,4.4]);

fun filter (P,nil) = nil
| filter (P,x::xs) =
    if P(x) then x::filter(P,xs)
        else filter (P,xs);

filter((fn(x) => x > 0),[1,2,~2,~6,7]);

fun readAndSum infile =
    if TextIO.endOfStream infile then
        0
    else
        case TextIO.inputLine infile of
        NONE => 0
        | SOME line => (case Int.fromString line of NONE => readAndSum infile
        | SOME n => n + readAndSum infile);