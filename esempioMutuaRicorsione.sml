(*take prende ogni indice dispari di una lista, skip è una funzione ausiliaria*)
fun take(x)=
    if x = [] then [] else hd(x)::skip(tail(x));
and skip(x)
    if x = [] then [] else take(tail(x));