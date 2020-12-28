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
# La surcharge de fonctions
TODO

# Les types énumérés
### Changes in **lexer.ml** :
* add tokens : {enum, ','} :
```
| "enum"    {ENUMERATION}
|  ","      {COMMA}
| ['A'-'Z'](['A'-'Z''a'-'z''0'-'9']|"-"|"_")* as n
    {TID n}
```
### Changes in **parser.mly** :
```
main : lenu = enums lfi = prog EOF     {let (Programme (_,lf1,li))=lfi in (Programme (lenu,lf1,li))}

enums : en1 = enum en2 = enums {en1::en2}
      |                          {[]}

enum : ENUMERATION n = TID AO x = ids AF PV   {Enumeration(n,x)}

ids : n=TID {[n]}
    | n=TID COMMA x=ids {n::x}
```



