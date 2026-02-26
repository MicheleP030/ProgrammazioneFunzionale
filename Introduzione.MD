# Meta Language

## Caratteristiche 
> 2. basato sulle funzioni, sono sia valori sia codice

3. funzioni superiori possono prendere funzioni come parametri

> 4. fortemente incoraggiata la ricorsione

5. pattern matching 

> 6. no side effects, le variabili non sono modificabili (non esiste assegnamento)

7. strong typing, determinati a compilazione

> 8. polimorfismo, si possono usare argomenti di tipo generico

9. dati astratti

## Esecuzione programmi


``` poly ``` -> esegue poly, facendo entrare nell'ambiente

``` rlwrap poly ``` -> esegue poly, mantiene la memoria dei comandi all'interno di poly
  
``` use "foo"; ``` -> esegue il file mentre dentro l'ambiente
  
``` poly < foo ``` -> esegue il file poly

## Espressioni e Comandi

### Espressioni
entità la quale valutazione produce un valore oppure non termina
 - una variabile o costante
 - un operatore su argomenti
ci sono tre notazioni possibili
 1. infissa, a + b
 2. prefissa \+ a b 
 3. suffissa a b \+
  
la seconda e la terza sono comode perchè non hanno bisogno di parentesi  

gli operatori vengo visti come funzioni  
il + prende come parametri a e b  
#### Tipi di evaluation
eager evaluation -> prima valuta tutti gli operandi e poi gli applica l'operatore     
(con eccezioni come if con ||, risulterebbero inutili)  
lazy evaluation -> valuta gli operandi solo quando necessario  

### Comandi
entità la quale valutazione non produce un valore ma ha un side effect   
(non esistenti nella programmazione funzionale)

## Tipi di dati
collezione di valori con operatori applicabili al loro tipo  
utili per:
* organizzare le info
* identificare errori
* ottimizzazione 

Tipi base:
 - int per il negativo uso la tilde, posso usare anche la notazione esadecimale  
    * val it = 3:int;
    * val it = ~3:int;
 - real per rendere reale deve avere almeno una cifra dopo il ., posso anche usare la notazione esponenziale  
    * val it = 3E2;
 - bool true o false  
 - char bisogna sempre usare #
    * val it = #"a":char;
 - string senza #
    * val it = "a":string;
unit (per espressioni senza valore, vale ())

### Espressioni
tutte le espressioni finiscono con ;  
1+2*3; diventa
> ```val it = 7: int;```
if <p> then <esp1> else <esp2>;
anche else è obbligatorio, e i tipi in then e else devono essere uguali
if 5<5 then 3 else 4;

### Operatori
#### 
\+ \- \* / div(divisione interi) mod(%) ~(rende negativo) ^(concatena stringhe)
\+ (a,b) svolge la somma della tupla, il + può prendere solo duple (int*int -> int)

#### Comparazioni

= < > <= >= <>

#### Operatori Logici
questi usano la lazy evaluation
not andalso(&&) orelse(||)