
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

(* b = false -> LOAD, b = true -> STORE *)
let rec analyse_code_affectable a b =
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
                            else failwith "erreur interne"
       | _ -> failwith "erreur interne"
    end
  | Valeur a1 -> if b then (analyse_code_affectable a1 false) ^ "STOREI (" ^ (string_of_int (aux a1)) ^ ")\n"
                 else (analyse_code_affectable a1 b) ^ "LOADI (" ^ (string_of_int (aux a1)) ^ ")\n"


let rec analyse_code_expression e =
  match e with 
  | AppelFonction (info, le) ->
    begin
      match info_ast_to_info info with 
      | InfoFun (n, _, _) -> 
        (List.fold_right (fun e acc -> (analyse_code_expression e) ^ acc) le "" ) ^ 
        "CALL (SB) " ^ n ^ "\n"
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
      | EquInt -> "SUBR IEq\n"
      | EquBool -> "SUBR BEq\n"
      | Inf -> "SUBR ILss\n"
      | EquEnum -> "SUBR IEq\n"
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
  | Empty -> ""

and analyse_code_bloc b =
  let taille = List.fold_right (fun i ti -> (taille_variables_declarees i) + ti) b 0 in
  let popfinal = "POP (0) " ^ (string_of_int taille) ^ "\n" in 
  (analyse_code_li b) ^ popfinal

and analyse_code_li li = String.concat "" (List.map analyse_code_instruction li)

let analyse_code_fonction (AstPlacement.Fonction(info, _, li, e)) = 
  match info_ast_to_info info with 
  | InfoFun(non, typeRet, typeParams) -> 
    let taille_variables_locales = List.fold_right (fun i ti -> (taille_variables_declarees i) + ti) li 0 in 
    let taille_return = getTaille typeRet in
    let taille_parametres = List.fold_right (fun i ti -> ti + (getTaille i) ) typeParams 0  in
    let nli = (analyse_code_li li) in
    non ^ "\n" ^
    nli ^
    (analyse_code_expression e) ^
    "POP (" ^ (string_of_int taille_return) ^ ") " ^ (string_of_int taille_variables_locales) ^ "\n" ^
    "RETURN (" ^ (string_of_int taille_return) ^ " )" ^ (string_of_int taille_parametres) ^ "\n"
  | _ -> failwith "erreur interne"

let analyse_code_valeur info = 
  match info_ast_to_info info with 
  | InfoVar(_, t, dep, reg) -> 
    "PUSH " ^ (string_of_int (getTaille t)) ^ "\n" ^
    "LOADL " ^ (string_of_int dep) ^ "\n" ^
    "STORE (" ^ (string_of_int (getTaille t)) ^ ") " ^ (string_of_int dep) ^ "[" ^ reg ^ "]\n" 
  | _ -> failwith "internal error"

let analyse_code_enumeration (AstType.Enumeration(_, ln)) = 
  String.concat "" (List.map analyse_code_valeur ln)

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

