# Prolog
Non descrive come risolvere un problema, ma quali fatti e regole sono veri

Fatti -> informazioni vere
Regole -> relazioni logiche
Query -> domande al sistema
``` prolog
padre(mario, luca).
padre(mario, anna).

figlio(X, Y) :- padre(Y, X).
```
in questo caso padre(mario, luca). è un fatto mentre figlio(X, Y) :- padre(Y, X). è una regola (X è figlio di Y se Y è padre di X)
```prolog
?- figlio(luca, mario).
```
questo invece è una query, che controlla se le info sono vere o no

si dice ground una funzione senza variabili (parole con iniziali maiuscole o _)

posso anche avere più requirement perchè una funzione sia true
```prolog
nonno(X, Y) :-
    genitore(X, Z),
    genitore(Z, Y).
```
    
usando "trace." nel terminale attivo il trace dell'albero di chiamate per visualizzarle
## Unificazione e Backtracking
Per unificazione si intende cercare di far combaciare termini e variabili, backtracking invece vuol dire che se una strada fallisce allora ne prova un altra
```prolog
colore(mela, rosso).
colore(banana, giallo).
colore(uva, viola).

?- colore(X, giallo).
```
se metto tra parentesi una variabile con la lettera iniziale maiuscola allora controllerà tra i fatti per trovare a quale mi sto riferendo 

non posso mettere le query nei file ".pl" che runno con prolog file.pl, invece dovrò 
esempio di backtraking, se non trovo X nella head della lista torno indietro e controllo se è nella tail
se il primo match è false allora va al secondo..., quando finisce tutti i path possibili (un albero) ritorna false
```prolog
unknown (X, [ H | T ] ) :- X=H.
unknown (X, [ H | T ] ) :- unknown(X, T).
```
## Liste 
come in ml posso accedere agli elementi tramite testa e coda:
[1,2,3,4] -> [testa (1)|coda (2,3,4)] 
possono essere disomogenee, e se faccio una query con solo una lista è true
```prolog
membro(X, [X|_]).
membro(X, [_|Coda]) :-
    membro(X, Coda).
```

## Operatore cut
L'operatore "!", cut, ferma il backtracking "congelando" tutto quello che succede prima (ad esempio se voglio controllare se a è dentro una lista se non metto ! ciclerà tutta la lista, in questo caso usando risorse in più inutilmente)
```prolog
max(X, Y, X) :- !.
max(X, Y, X) :- X >= Y.


max(_, Y, Y).
```
in questo caso la seconda variabile X è quella per ritornare il valore (max(primo, secondo, risultato))