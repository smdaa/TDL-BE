(* Module de la passe de placemment *)
module PassePlacementRat : Passe.Passe with type t1 = Ast.AstType.programme and type t2 = Ast.AstPlacement.programme =
struct

  open Tds
  open Type
  open Ast
  open AstPlacement

  type t1 = Ast.AstType.programme
  type t2 = Ast.AstPlacement.programme

let rec analyse_placement_instruction i base reg =
  match i with 
  | AstType.Declaration (_, info) -> 
    begin
    match info_ast_to_info info with 
    | InfoVar(_, t, _, _) -> 
      modifier_adresse_info base reg info;
      getTaille t 
    | _ -> failwith "erreur interne"
    end
  | AstType.Conditionnelle (_, t, e) ->
    analyse_placement_bloc t base reg;
    analyse_placement_bloc e base reg;
    0
  | AstType.TantQue (_, b) -> analyse_placement_bloc b base reg; 0
  | _ -> 0

and analyse_placement_bloc li base reg = 
  let _ = List.fold_left (fun t qt -> t + (analyse_placement_instruction qt t reg)) base li in ()

let analyse_placement_parametre info base =
  match info_ast_to_info info with
  | InfoVar (_,t, _, _) -> 
    modifier_adresse_info (base - getTaille t) "LB" info ;
    getTaille t
  | _ -> failwith "erreur interne"

let analyse_placement_parametres lp = 
  List.fold_left (fun d p -> d + analyse_placement_parametre p (-d)) 0 (List.rev lp)

let analyse_placement_fonction (AstType.Fonction(info, lp, li, e)) = 
  let _ = analyse_placement_parametres lp in 
  analyse_placement_bloc li 3 "LB";
  Fonction(info, lp, li, e)

let analyser (AstType.Programme(fonctions, prog)) =
  let lf = List.map analyse_placement_fonction fonctions in
  let _ = analyse_placement_bloc prog 0 "SB" in
  Programme(lf, prog)
end