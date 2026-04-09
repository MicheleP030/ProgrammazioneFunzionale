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

fun returnThird(x::y::z::w) = print(Int.toString(z))
    | returnThird(_) = raise shortList 3; (*lancio l eccezione con le info sulla lunghezza*)

(*visto che returnThird printa, devo printare anche nel handle (il valore in caso di errore deve essere lo stesso di quello senza errore)*)
val x = returnThird([1]) handle shortList n => print(Int.toString(n)); (*gestisco l'eccezione per qualsiasi n*)

exception negativo of int

fun fact(0) = 0
    | fact (1) = 1
    | fact (n) = if n>0 then n*fact(n-1) else raise negativo n;

val y = fact(~5) handle negativo n => n; (*y deve essere di tipo int quindi non posso mettere il print qua*)
print(Int.toString(y));

(*se voglio che all errore della funzione succeda qualcosa come un print allora posso usare unit come tipo di ritorno dell errore*)
(*exception errore of unit*int => raise errore (print("ciao"),0)*)

fun tabulate (a,k,n,i,F) = 
    if(i=n)
        (print("finitos"), print(""))
    else 
        (print(Real.toString(a)^" "^Real.toString(F(x))); tabulate(a+k,k,n,i+1,F));