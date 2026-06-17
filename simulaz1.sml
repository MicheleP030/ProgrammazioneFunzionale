(*Si scriva una funzione prefix che riceve come argomento una stringa e restituisce una lista di stringhe contenente tutti i prefissi della stringa
Es: "Ciao" -> ["Ciao","Cia","Ci,"C"] 
(no la stringa vuota)
Consiglio: nella consegna originale si consigliava di usare la funzione "map", ma questa soluzione non la usa*)

(*una funzione dice quante ne servono, fino al max e una costruisce la mappa e richiama*)

fun length(nil) = 0
    |  length (l::lx) = 1+length(lx);

fun prefix_help (nil,cn) = nil
    | prefix_help (s:char list,cn) = if cn = 0 then [] else hd(s)::prefix_help(tl(s),cn-1);

fun prefix2 (s:char list,n) = if length(s) >= n then implode(prefix_help(s,n))::prefix2(s,n+1) else [];

(*aggiunge la current parola, cn è current lenght*)
fun prefix (s) = prefix2(explode(s),1);


prefix("Ciao");




