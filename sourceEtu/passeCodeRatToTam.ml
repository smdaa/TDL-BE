
(* Module de la passe de génération de code *)
module PasseCodeRatToTam : Passe.Passe with type t1 = Ast.AstPlacement.programme and type t2 = string =
struct
  open Tds
  open Code
  open Type
  open Ast
  open AstType

  type t1 = Ast.AstPlacement.programme
  type t2 = string


(* analyse_code_affectable : AstType.affectable -> boolean -> string*)
(* Paramètre a : l'affectable à analyser *)
(* Paramètre b = false -> LOAD, b = true -> STORE *)
(* génerer le code tam d'un affectable *)
let rec analyse_code_affectable a b =
  (* fonction auxiliere :  AstType.affectable -> int *)
  (* Paramètre a : l'affectable dont on cherche la taille *)
  (* renvoie la taille de la valeur pointé par un pointeur *)
  let rec aux a =
    match a with 
    | Valeur a1 -> aux a1
    | Ident info -> 
      begin
        match info_ast_to_info info with 
        | InfoVar(_, t, _, _) -> 
          begin 
            match t with 
            | Pointeur t1 ->  getTaille t1 
            | _ -> failwith "internal error"
          end
        | _ -> failwith "internal error"
      end
  in 
  match a with 
  | Ident info ->
    begin
       match info_ast_to_info info with 
       | InfoVar(_, t, dep, reg) -> if b then ("STORE (" ^ (string_of_int (getTaille t)) ^ ") " ^ (string_of_int dep) ^ "[" ^ reg ^ "]\n") 
                                    else ("LOAD (" ^ (string_of_int (getTaille t)) ^ ") " ^ (string_of_int dep) ^ "[" ^ reg ^ "]\n")
       | InfoConst (_, i) -> if not b then "LOADL " ^ (string_of_int i) ^ "\n"
                            else failwith "internal error"
       | _ -> failwith "internal error"
    end
  | Valeur a1 -> if b then (analyse_code_affectable a1 false) ^ "STOREI (" ^ (string_of_int (aux a1)) ^ ")\n"
                 else (analyse_code_affectable a1 b) ^ "LOADI (" ^ (string_of_int (aux a1)) ^ ")\n"


(* analyse_code_expression : AstType.expression -> string*)
(* Paramètre e : l'expression à analyser *)
(* génerer le code tam d'une expression *)
let rec analyse_code_expression e =
  match e with 
  | AppelFonction (info, le, tle) ->
    begin
      match info_ast_to_info info with 
      | InfoFun (n, _, _) -> 
        (List.fold_right (fun e acc -> (analyse_code_expression e) ^ acc) le "" ) ^ 
        "CALL (SB) " ^ n ^ (List.fold_right (fun h t -> (string_of_type h) ^ t) tle) "" ^ "\n"
      | _ -> failwith "internal error"
    end
  | Rationnel (e1, e2) -> (analyse_code_expression e1) ^
                          (analyse_code_expression e2)
  | Numerateur e1 -> (analyse_code_expression e1) ^ "POP (0) 1\n"
  | Denominateur e1 -> (analyse_code_expression e1) ^ "POP (1) 1\n"
  | True -> "LOADL 1\n"
  | False -> "LOADL 0\n"
  | Entier i -> "LOADL " ^(string_of_int i) ^ "\n"
  | Binaire (b, e1, e2) ->
    (analyse_code_expression e1) ^ 
    (analyse_code_expression e2) ^
    begin
      match b with 
      | PlusInt -> "SUBR IAdd\n"
      | MultInt -> "SUBR IMul\n"
      | PlusRat -> "CALL (SB) RAdd\n"
      | MultRat -> "CALL (SB) RMul\n"
      (* l'égalité des deux valeur de type enum est l'égalité de leurs adresses *)
      | EquInt | EquEnum -> "SUBR IEq\n"
      | EquBool -> "SUBR BEq\n"
      | Inf -> "SUBR ILss\n"
    end
  | Null  -> "SUBR MVoid\n"
  | New t -> "LOADL " ^ (string_of_int (getTaille t)) ^ "\n" ^
              "SUBR MAlloc\n"
  | Adresse info ->
    begin
      match info_ast_to_info info with 
      | InfoVar(_, _, dep, reg) -> "LOADA " ^ (string_of_int dep) ^ " [" ^ reg ^ "]\n"
      | _ -> failwith "internal error" 
    end
  | Affectable a -> (analyse_code_affectable a false)
  | Tident n -> 
    begin
      match info_ast_to_info n with 
      | InfoVar (_, t, dep, reg) -> ("LOAD (" ^ (string_of_int (getTaille t)) ^ ") " ^ (string_of_int dep) ^ "[" ^ reg ^ "]\n")
      | _ -> failwith "erreur interne"
    end
  | Default -> "\n"


(* analyse_code_instruction : AstType.instruction -> string*)
(* Paramètre e : l'instruction à analyser *)
(* génerer le code tam d'une instruction *)
let rec analyse_code_instruction i =
  match i with 
  | Declaration (e, info) ->
    begin 
      match info_ast_to_info info with 
      | InfoVar (_, t, dep, reg) -> 
        let taille = string_of_int (getTaille t) in
        "PUSH " ^ taille ^ "\n" ^
        (analyse_code_expression e) ^
        "STORE (" ^ taille ^") " ^ (string_of_int dep) ^ "[" ^ reg ^ "]\n"
      | _ -> failwith "erreur interne"
    end
  | Affectation (a, e) ->
    (analyse_code_expression e) ^
    (analyse_code_affectable a true)
  | AffichageInt(e) -> (analyse_code_expression e) ^ "SUBR IOut\n"
  | AffichageBool(e) -> (analyse_code_expression e) ^ "SUBR BOut\n"
  | AffichageRat(e) -> (analyse_code_expression e) ^ "CALL (SB) ROut\n"
  | Conditionnelle (c, t, e) -> 
    let label_else = getEtiquette() in
    let label_endif = getEtiquette() in
    (analyse_code_expression c) ^
    "JUMPIF (0) " ^ label_else ^ "\n" ^
    (analyse_code_bloc t) ^
    "JUMP " ^ label_endif ^ "\n" ^
    label_else ^ "\n" ^
    (analyse_code_bloc e) ^
    label_endif ^ "\n"
  | TantQue (c, b) ->
    let label_debut = getEtiquette() in
    let label_fin = getEtiquette() in
    label_debut ^ "\n" ^
    (analyse_code_expression c) ^
    "JUMPIF (0) " ^ label_fin ^ "\n" ^
    (analyse_code_bloc b) ^
    "JUMP " ^ label_debut ^ "\n" ^
    label_fin^"\n"
  | Break -> "\n"
  | Switch (e,lc) -> 
    (* fonction auxiliere : string -> (AstType.expression * AstType.bloc * AstType.instruction) -> string *)
    (* Paramètre l : etiquette pour quitter le switch en cas de break*)
    (* Paramètre b : code bloc du cas n (case n : bloc) *)
    (* Paramètre i : instruction a la fin du bloc (Break ou empty) *)
    (* génerer le code tam d'un cas n avec sortie du bloc en cas de break *)
    let aux l (_, b, i) = 
      let label = getEtiquette() in
      match i with 
      | Break -> 
        label ^ "\n" ^
        (analyse_code_bloc b) ^
        "JUMP " ^ l ^ "\n" 
      | Empty -> 
        label ^ "\n" ^
        (analyse_code_bloc b) 
      | _ -> failwith "internal error"
    in
    (* fonction auxiliere1 : AstType.expression -> AstType.binaire *)
    (* Paramètre e : expression *) 
    (* renvoie l'opération binaire equivalent a l'expression*)
    let aux1 e = 
      match e with 
        | True | False -> EquBool
        | Entier _ -> EquInt
        | Tident _ -> EquEnum
        | Default -> MultRat (* le choix de MultRat est arbitraire *)
        | _ -> failwith "internal error"
    in
    begin
      match lc with 
      | [] -> "\n"
      | (e1, b1, _)::t -> 
        let label_end_switch = getEtiquette() in
        let label_ok = getEtiquette() in (* en cas d'égalité e avec e1 *)
        let label_ko = getEtiquette() in (* en cas d'inégalité e avec e1 *)
        let bin = aux1 e1 in
        if not(bin = MultRat) then
          (analyse_code_expression (Binaire(bin, e, e1))) ^
          "JUMPIF (0) " ^ label_ko ^ "\n" ^
          String.concat "" (List.map (aux label_end_switch) lc) ^ "\n" ^
          "JUMP " ^ label_ok ^ "\n" ^
          label_ko ^ "\n" ^
          (analyse_code_instruction (Switch (e, t))) 
          ^ label_ok ^ "\n" ^
          "\n" ^ label_end_switch ^ "\n"
        else (* le cas ou e1=Default *)
          (analyse_code_bloc b1) ^
          "\n" ^ label_end_switch ^ "\n"
    end
  | Empty -> "\n"


(* analyse_code_bloc : AstType.bloc -> string*)
(* Paramètre b : le bloc à analyser *)
(* génerer le code tam d'un bloc en ajouter la liberation de l'espace a la fin*)
and analyse_code_bloc b =
  let taille = List.fold_right (fun i ti -> (taille_variables_declarees i) + ti) b 0 in
  let popfinal = "POP (0) " ^ (string_of_int taille) ^ "\n" in 
  (analyse_code_li b) ^ popfinal


(* analyse_code_li : AstType.bloc -> string*)
(* Paramètre li : le bloc à analyser *)
(* génerer le code tam d'un bloc *)
and analyse_code_li li = String.concat "" (List.map analyse_code_instruction li)


(* analyse_code_fonction : AstType.fonction -> string*)
(* Paramètre (AstPlacement.Fonction(info, _, li, e) : la fonction à analyser *)
(* génerer le code tam d'une fonction *)
let analyse_code_fonction (AstPlacement.Fonction(info, lp, li, e)) = 
  let aux ia = 
    match info_ast_to_info ia with 
    | InfoVar (_, t, _, _) -> t
    | _ -> failwith "internal error"
  in
  match info_ast_to_info info with 
  | InfoFun(non, typeRet, _) -> 
    let ltp = List.map aux lp in
    let taille_variables_locales = List.fold_right (fun i ti -> (taille_variables_declarees i) + ti) li 0 in 
    let taille_return = getTaille typeRet in
    let taille_parametres = List.fold_right (+) (List.map getTaille ltp) 0 in
    let nli = (analyse_code_li li) in
    non ^ (List.fold_right (fun h t -> (string_of_type h) ^ t) ltp "") ^ "\n" ^
    nli ^
    (analyse_code_expression e) ^
    "POP (" ^ (string_of_int taille_return) ^ ") " ^ (string_of_int taille_variables_locales) ^ "\n" ^
    "RETURN (" ^ (string_of_int taille_return) ^ " )" ^ (string_of_int taille_parametres) ^ "\n"
  | _ -> failwith "erreur interne"


(* analyse_code_valeur : info_ast -> string *)
(* Paramètre info :  info_ast du valeur à analyser *)
(* génerer le code tam d'une valeur de type enumeration *)
let analyse_code_valeur info = 
  match info_ast_to_info info with 
  | InfoVar(_, t, dep, reg) -> 
    "PUSH " ^ (string_of_int (getTaille t)) ^ "\n" ^
    "LOADL " ^ (string_of_int dep) ^ "\n" ^
    "STORE (" ^ (string_of_int (getTaille t)) ^ ") " ^ (string_of_int dep) ^ "[" ^ reg ^ "]\n" 
  | _ -> failwith "internal error"


(* analyse_code_enumeration : AstType.enumeration -> string *)
(* Paramètre AstType.Enumeration(_, ln) :  l'enumeration a analyser *)
(* génerer le code tam d'une enumeration *)
let analyse_code_enumeration (AstType.Enumeration(_, ln)) = 
  String.concat "" (List.map analyse_code_valeur ln)


(* analyser : AstPlacement.ast -> string *)
(* Paramètre : le programme à analyser *)
(* generer le code d'un programme *)
let analyser (AstPlacement.Programme(enumerations,fonctions, bloc)) = 
  let ne = String.concat "" (List.map analyse_code_enumeration enumerations) in
  let lf = String.concat "" (List.map analyse_code_fonction fonctions) in
  let b = (analyse_code_bloc bloc) in
  getEntete () ^
  lf ^ "\n" ^
  "main\n" ^ 
  ne ^ "\n" ^ 
  b ^ "\n" ^
  "HALT\n"
end
