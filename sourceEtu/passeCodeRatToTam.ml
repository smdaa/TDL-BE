(* Module de la passe de génération de code *)
module PasseCodeRatToTam : Passe.Passe with type t1 = Ast.AstPlacement.programme and type t2 = string =
struct
  open Tds
  open Code
  open Type
  open Ast
  open AstPlacement
  open AstType

  type t1 = Ast.AstPlacement.programme
  type t2 = string

let rec analyse_code_expression e =
  match e with 
  | AppelFonction (info, le) -> failwith "TODO"
  | Rationnel (e1, e2) -> (analyse_code_expression e1) ^ (analyse_code_expression e2)
  | Numerateur e1 -> (analyse_code_expression e1) ^ "POP (0) 1\n"
  | Denominateur e1 -> (analyse_code_expression e1) ^ "POP (1) 1\n"
  | Ident info ->
    begin
      match info_ast_to_info info with 
      | InfoVar(_, t, dep, reg) -> failwith "TODO"
      | InfoConst _ -> failwith "TODO"
      | _ -> failwith "erreur interne"
    end
  | True -> "LOADL 1\n"
  | False -> "LOADL 0\n"
  | Entier i -> "LOADL " ^(string_of_int i) ^ "\n"
  | Binaire (b, e1, e2) ->
    (analyse_code_expression e1) ^ (analyse_code_expression e2)
    ^
    begin
      match b with 
      | PlusInt -> "SUBR IAdd\n"
      | PlusRat -> "CALL (SB) RAdd\n"
      | MultInt -> "SUBR IMul\n"
      | MultRat -> "CALL (SB) RMul\n"
      | EquInt -> "SUBR IEq\n"
      | EquBool -> "SUBR IEq\n"
      | Inf -> "SUBR ILss\n"
    end

let rec analyse_code_instruction i =
  match i with 
  | Declaration (e, info) ->
    begin 
      match info_ast_to_info info with 
      | InfoVar _ -> 
        let taille = string_of_int (getTaille (get_type info)) in
        "PUSH "^taille^"\n"
        ^(analyse_code_expression e)
        ^"STORE ("^taille^") "^(string_of_int (get_dep info))^"["^(get_reg info)^"]\n"
      | _ -> failwith "erreur interne"
    end
  | Affectation (e, info) ->
    begin 
      match info_ast_to_info info with 
      | InfoVar _ -> 
        (analyse_code_expression e)
      | _ -> failwith "erreur interne"
    end
  | AffichageInt(e) -> (analyse_code_expression e) ^ "SUBR IOut\n"
  | AffichageBool(e) -> (analyse_code_expression e) ^ "SUBR BOut\n"
  | AffichageRat(e) -> (analyse_code_expression e) ^ "CALL (SB) ROut\n"
  | Conditionnelle (c, t, e) ->
    let x = getEtiquette () in
    let y = getEtiquette () in
    (analyse_code_expression c) ^ "JUMPIF (0) " ^ x ^ "\n" ^(analyse_code_bloc t) ^ "JUMP " ^ y ^ "\n" ^ x ^ "\n" ^ (analyse_code_bloc e) ^ y ^ "\n"
  | TantQue (c, b) ->
    let debut_tq = getEtiquette () in 
    let fin_tq = getEtiquette () in
    debut_tq ^ "\n" ^ (analyse_code_expression c) ^ "JUMPIF (0) " ^ fin_tq ^ "\n" ^ (analyse_code_bloc b) ^ "JUMP " ^ debut_tq ^ "\n" ^ fin_tq ^ "\n"
  | Empty -> ""

and analyse_code_bloc b =
  let taille = List.fold_right (fun i ti -> (taille_variables_declarees i) + ti) b 0 in
  let popfinal = "POP (0) " ^ (string_of_int taille) ^ "\n" in 
  (analyse_code_bloc b) ^ popfinal

let analyse_code_fonction (Fonction(info, _, li, e)) = failwith "TODO"

let analyser (AstPlacement.Programme(fonctions, bloc)) = failwith "TODO"

end