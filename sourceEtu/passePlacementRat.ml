
(* Module de la passe de placemment *)
module PassePlacementRat : Passe.Passe with type t1 = Ast.AstType.programme and type t2 = Ast.AstPlacement.programme =
struct

  open Tds
  open Type
  open Ast
  open AstPlacement
  (*open Format*)

  type t1 = Ast.AstType.programme
  type t2 = Ast.AstPlacement.programme


(* analyse_placement_instruction : AstType.instruction -> int -> string -> int *)
(* Paramètre i : l'instruction à analyser *)
(* Paramètre base : adresse courante *)
(* Paramètre reg : registre courant *)
(* retourne le deplacement de l'adresse apres l'instruction et modife
les adresses et les registres des variables dans la structure tds*)
let rec analyse_placement_instruction i base reg =
  match i with 
  | AstType.Declaration (_, info) -> 
    begin
      match info_ast_to_info info with 
      | InfoVar(_, t, _, _) -> 
        modifier_adresse_info base reg info;
        (*let () = printf "%s adresse %d SB \n" n (base) in *)
        getTaille t 
      | _ -> failwith "internal error"
    end
  | AstType.Conditionnelle (_, t, e) ->
    analyse_placement_bloc t base reg;
    analyse_placement_bloc e base reg;
    0
  | AstType.TantQue (_, b) -> analyse_placement_bloc b base reg; 
    0
  | AstType.Switch(_, lc) -> 
    let aux (_, b, _) = 
      analyse_placement_bloc b base reg;
    in 
    let _ = List.map (aux) lc in
    0
  | _ -> 0


(* analyse_placement_bloc : AstType.bloc -> int -> string -> unit *)
(* Paramètre li : le bloc  à analyser *)
(* Paramètre base : adresse courante *)
(* Paramètre reg : registre courant *)
(* effectuer l'analyse placement mémoire pour un bloc *)
and analyse_placement_bloc li base reg = 
  let _ = List.fold_left (fun t qt -> t + (analyse_placement_instruction qt t reg)) base li in ()


(* analyse_placement_parametre : info_ast -> int -> int*)
(* Paramètre info : info_ast du parametre à analyser *)
(* Paramètre base : adresse courante *)
(* effectuer l'analyse placement mémoire pour un paramtre d'une fonction*)
let analyse_placement_parametre info base =
  match info_ast_to_info info with
  | InfoVar (_,t, _, _) -> 
    modifier_adresse_info (base - getTaille t) "LB" info ;
    getTaille t
  | _ -> failwith "internal error"


(* analyse_placement_parametres : info_ast list-> int *)
(* Paramètre lp : liste des info_ast des parametres à analyser *)
(* effectuer l'analyse placement mémoire pour les parametres d'une fonction*)
let analyse_placement_parametres lp = 
  List.fold_left (fun d p -> d + analyse_placement_parametre p (-d)) 0 (List.rev lp)


(* analyse_placement_fonction : AstType.fonction -> AstPlacement.fonction *)
(* Paramètre AstType.Fonction(info, lp, li, e) : fonction à analyser *)
(* effectuer l'analyse placement mémoire pour une fonction et le transforme en un AstPlacemment.fonction *)
let analyse_placement_fonction (AstType.Fonction(info, lp, li, e)) = 
  let _ = analyse_placement_parametres lp in 
  analyse_placement_bloc li 3 "LB";
  Fonction(info, lp, li, e)


(* analyse_placement_valeur : info_ast -> int -> int *)
(* Paramètre info :  info_ast du valeur à analyser *)
(* Paramètre base : adresse courante *)
(* effectuer l'analyse placement mémoire pour une valeur de type enumeration*)
let analyse_placement_valeur info base = 
  match info_ast_to_info info with 
  | InfoVar (_,t, _, _) -> 
    modifier_adresse_info (base + getTaille t) "SB" info ;
    (*let () = printf "%s adresse %d SB \n" n (base + getTaille t) in *)
    getTaille t
  | _ -> failwith "erreur interne"


(* analyse_placement_enumeration : AstType.enumeration -> int -> int *)
(* Paramètre base : adresse courante *)
(* Paramètre AstType.Enumeration(_,ln) :  l'enumeration a analyser *)
(* effectuer l'analyse placement mémoire pour une enumeration déclaree*)
let analyse_placement_enumeration base (AstType.Enumeration(_,ln)) = 
  let d = List.fold_left (fun d info -> d + analyse_placement_valeur info (d)) (base) ln in 
  d + 1


(* analyse_placement_enumerations : AstType.enumeration list -> AstType.enumeration list *)
(* Paramètre enumerations : la liste des enumerations a analyser *)
(* effectuer l'analyse placement mémoire pour une liste d'enumerations*)
let analyse_placement_enumerations enumerations =
  let _ = List.fold_left (fun d en -> d + (analyse_placement_enumeration d en)) (-1) enumerations in 
  enumerations

(* get_nombre_enum : AstType.enumeration list -> int*)
(* Paramètre enumerations : la liste des enumerations *)
(*renvoie combien on a de valeur de type enumerations on a dans un programme *)
let get_nombre_enum enumerations = 
  List.fold_right (fun h t -> let AstType.Enumeration(_,ln) = h in  t + List.length ln) enumerations 0


(* analyser : AstType.ast -> AstPlacement.ast *)
(* Paramètre : le programme à analyser *)
(* effectuer l'analyse placement mémoire pour un programme
et le transforme en un AstPlacemment.programme *)
let analyser (AstType.Programme(enumerations, fonctions, prog)) =
  let _ = analyse_placement_enumerations enumerations in 
  let lf = List.map analyse_placement_fonction fonctions in
  let _ = analyse_placement_bloc prog (get_nombre_enum enumerations) "SB" in
  Programme(enumerations, lf, prog)
end
