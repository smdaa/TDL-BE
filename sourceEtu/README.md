# Pointeurs
### Changes in **lexer.ml** :
* add tokens : {&, new, null} :
```
| "&"       {AMV} 
| "null"    {NULL}
| "new"     {NEW}
```
### Changes in **parser.mly** :
* add rule :  TYPE -> TYPE *
```
| t=typ MULT {Pointeur(t)}
```
* change rule :  I -> A = E 
```
| n=a EQUAL e1=e PV   {Affectation (n,e1)}
```
* add rule :  E -> A 
```
| n=a   {Affectable(n)}
```
* add rule :  E -> null 
```
| NULL   {NULL}
```
* add rule :  E -> new TYPE 
```
| PO NEW t=typ PF   {NEW (t)}
```
* add rule :  E -> & id 
```
| AMV n=ID  {Adresse (n)}
```
* add rule :  A -> * A 
```
| PO Mult n=a PF   {Valeur (n)}
```
* add rule :  A -> id 
```
| n=ID   {Ident n}
```
### Changes in **type.ml** :
add typ Pointeur of typ 
```
type typ = Bool | Int | Rat | Undefined | Pointeur of typ
```