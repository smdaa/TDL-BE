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
# La structure de contrˆole switch/case
### Changes in **lexer.ml** :
```
| "switch"  {SWITCH}
| "case"    {CASE}
| "default" {DEFAULT}
| ":"       {DBPT}
| "break"   {BREAK}
```
### Changes in **parser.mly** :
```
i :
| t=typ n=ID EQUAL e1=e PV          {Declaration (t,n,e1)}
| n=a EQUAL e1=e PV                {Affectation (n,e1)}
| CONST n=ID EQUAL e=ENTIER PV      {Constante (n,e)}
| PRINT e1=e PV                     {Affichage (e1)}
| IF exp=e li1=bloc ELSE li2=bloc   {Conditionnelle (exp,li1,li2)}
| WHILE exp=e li=bloc               {TantQue (exp,li)}
| SWITCH PO exp=e PF AO lc1=lc AF {Switch (exp,lc1)}

lc :
| c1=c lc1=lc {c1::lc1}
| {[]}

c:
| CASE n=TID DBPT ls=is i=b {(Tident n,ls,i)}
| CASE e=ENTIER DBPT ls=is i=b {(Entier e,ls,i)}
| CASE TRUE DBPT ls=is i=b {(True,ls,i)}
| CASE FALSE DBPT ls=is i=b {(False,ls,i)}
| CASE DEFAULT DBPT ls=is i=b {(Default,ls,i)} 

b :
| {Notbreak}
| BREAK {Break}
```


