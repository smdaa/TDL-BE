
(* Module de la passe de typage *)
module PasseTypeRat : Passe.Passe with type t1 = Ast.AstTds.programme and type t2 = Ast.AstType.programme =
struct

  open Tds
  open Exceptions
  open Type
  open Ast
  open AstType

  type t1 = Ast.AstTds.programme
  type t2 = Ast.AstType.programme

(* analyse_type_affectable : AstTds.affectable -> typ * AstType.affectable *)
(* Paramètre a : l'affectable à analyser *)
(* Retourne le type de l'affectable  *)
let rec analyse_type_affectable a =
  match a with 
  | AstTds.Ident info ->
    begin
      match info_ast_to_info info with 
      | InfoVar (_, t, _, _) -> (t, Ident info)
      | InfoConst _ -> (Int, Ident info)
      | _ -> failwith "internal error"
    end
  | AstTds.Valeur a ->
    begin
      match (analyse_type_affectable a) with 
      | (Pointeur tp, na) -> (tp, Valeur na)
      | _ -> failwith "internal error"
    end

(* analyse_type_expression : AstTds.expression -> typ * AstType.expression *)
(* Paramètre e : l'expression à analyser *)
(* Vérifie la bonne utilisation des types  et tranforme l'expression
en une expression de type AstType.expression *)
(* Erreur si mauvaise utilisation des types *)
let rec analyse_type_expression e =
  match e with 
  | AstTds.AppelFonction (info, le) -> 
    let (tle, nle) = List.split (List.map analyse_type_expression le) in 
    begin
      match info_ast_to_info info with 
      | InfoFun (_, typeRet, typeParams) ->
        if (est_compatible_list tle typeParams) then (typeRet, AppelFonction(info, nle)) else raise (TypesParametresInattendus (tle, typeParams))
      | _ -> failwith "internal error"
    end
  | AstTds.Rationnel (e1, e2) ->
    let (te1, ne1) = analyse_type_expression e1 in
    let (te2, ne2) = analyse_type_expression e2 in
    begin
      if ((te1 = Int) && (te2 = Int)) then (Rat, Rationnel(ne1, ne2))
      else 
        if (te1 <> Int) then raise (TypeInattendu(te1, Int))
        else raise (TypeInattendu(te2, Int))
    end
  | AstTds.Numerateur e ->
    let (te, ne) = analyse_type_expression e in 
    if (te = Rat) then (Int, Numerateur ne)
    else raise (TypeInattendu (te, Rat))
  | AstTds.Denominateur e ->
    let (te, ne) = analyse_type_expression e in 
    if (te = Rat) then (Int, Denominateur ne)
    else raise (TypeInattendu (te, Rat))
  | AstTds.True -> (Bool, True)
  | AstTds.False -> (Bool, False)
  | AstTds.Entier i -> (Int, Entier i)
  | AstTds.Binaire (op, e1, e2) ->
    let (te1, ne1) = analyse_type_expression e1 in
    let (te2, ne2) = analyse_type_expression e2 in
    begin
    match te1,op,te2 with 
    | Int, Plus, Int -> (Int, Binaire(PlusInt, ne1, ne2))
    | Rat, Plus, Rat -> (Rat, Binaire(PlusRat, ne1, ne2))
    | Int, Mult, Int -> (Int, Binaire(MultInt, ne1, ne2))
    | Rat, Mult, Rat -> (Rat, Binaire(MultRat, ne1, ne2))
    | Int, Equ, Int  -> (Bool, Binaire(EquInt, ne1, ne2))
    | Bool, Equ, Bool -> (Bool, Binaire(EquBool, ne1, ne2))
    | Int, Inf, Int -> (Bool, Binaire(Inf, ne1, ne2))
    | _ -> raise(TypeBinaireInattendu(op,te1,te2))
    end
  | AstTds.Affectable a -> 
    let (ta, na) = analyse_type_affectable a in
    (ta, Affectable na)
  | AstTds.Null -> (Pointeur(Undefined), Null)
  | AstTds.New t -> (Pointeur(t), New (t))
  | AstTds.Adresse a -> 
    begin
      match info_ast_to_info a with
      | InfoVar(_,t,_,_) -> (Pointeur(t), Adresse a)
      | _ -> failwith "internal error"
    end

(* analyse_type_instruction : AstTds.instruction -> AstType.instruction *)
(* Paramètre i : l'instruction à analyser *)
(* Vérifie la bonne utilisation des types  et tranforme l'instruction
en une instruction de type AstType.instruction *)
(* Erreur si mauvaise utilisation des types *)
let rec analyse_type_instruction i = 
  match i with 
  | AstTds.Declaration (t, e, info) -> 
    let (te, ne) = analyse_type_expression e in
    if (est_compatible te t) then
      begin
        modifier_type_info t info;
        Declaration (ne, info)
      end
    else raise (TypeInattendu (te, t))
  | AstTds.Affectation (a, e) ->
    let (te, ne) = analyse_type_expression e in
    let (ta, na) = analyse_type_affectable a in
    if est_compatible te ta then
      Affectation (na, ne)
    else
      raise (TypeInattendu (te, ta))
  | AstTds.Affichage e ->
    let (te, ne) = analyse_type_expression e in
    begin
      match te with 
      | Int -> AffichageInt ne
      | Rat -> AffichageRat ne
      | Bool -> AffichageBool ne
      | _ -> failwith "internal error"
    end
  | AstTds.Conditionnelle (c, t, e) ->
    let (tc, nc) = analyse_type_expression c in
    begin
      match tc with 
      | Bool -> 
        let nt = analyse_type_bloc t in
        let ne = analyse_type_bloc e in
        Conditionnelle (nc, nt, ne)
      | _ -> raise (TypeInattendu (tc, Bool))
    end
  | AstTds.TantQue (c, b) ->
    let (tc, nc) = analyse_type_expression c in
    begin
      match tc with
      | Bool ->  
        let nb = analyse_type_bloc b in
        TantQue (nc, nb)
      | _ -> raise (TypeInattendu (tc, Bool))
    end
  | AstTds.Empty -> Empty

(* analyse_type_bloc : AstTds.bloc -> AstType.bloc *)
(* Paramètre b : liste d'instructions à analyser *)
(* Vérifie la bonne utilisation des types et tranforme le bloc
en un bloc de type AstType.bloc *)
(* Erreur si mauvaise utilisation des types *)
and analyse_type_bloc b = List.map analyse_type_instruction b

(* analyse_type_fonction : AstTds.fonction -> AstType.fonction *)
(* Paramètre : la fonction à analyser *)
(* Vérifie la bonne utilisation des types et tranforme la fonction
en une fonction de type AstType.fonction *)
(* Erreur si mauvaise utilisation des types *)
let analyse_type_fonction (AstTds.Fonction(t, info, lp, li, e))=
  let (type_lp, info_lp) = List.split lp in
  let () = modifier_type_fonction_info t type_lp info in  
  let nli = analyse_type_bloc li in 
  let (te, ne) = analyse_type_expression e in 
  if (est_compatible t te) then
    Fonction(info, info_lp, nli, ne)
  else raise (TypeInattendu (te, t))

(* analyser : AstTds.ast -> AstType.ast *)
(* Paramètre : le programme à analyser *)
(* Vérifie la bonne utilisation des types et tranforme le programme
en un programme de type AstType.fonction *)
(* Erreur si mauvaise utilisation des types *)
let analyser (AstTds.Programme (fonctions, prog)) = 
  let lf = List.map analyse_type_fonction fonctions in
  let b = analyse_type_bloc prog in
  Programme(lf,b)
end
