type typ = Bool | Int | Rat | Undefined | Pointeur of typ | Enum of string

let rec string_of_type t = 
  match t with
  | Bool ->  "Bool"
  | Int  ->  "Int"
  | Rat  ->  "Rat"
  | Undefined -> "Undefined"
  | Pointeur t -> "Pointeursur" ^ (string_of_type t)
  | Enum t -> "Enumerationde" ^ t

let%test _ = string_of_type (Pointeur Int) = "PointeursurInt"
let%test _ = string_of_type (Enum "Mois") = "EnumerationdeMois"
let%test _ = string_of_type Undefined = "Undefined"

let rec est_compatible t1 t2 =
  match t1, t2 with
  | Bool, Bool -> true
  | Int, Int -> true
  | Rat, Rat -> true
  | Undefined, Undefined -> true
  | Pointeur t1, Pointeur t2 -> est_compatible t1 t2
  | Enum en1, Enum en2 -> en1=en2
  | _ -> false 

let%test _ = est_compatible (Pointeur Int) (Pointeur Int) = true
let%test _ = est_compatible (Pointeur Int) (Pointeur Rat) = false
let%test _ = est_compatible (Enum "Mois") (Enum "Mois") = true
let%test _ = est_compatible (Enum "Mois") (Enum "Jour") = false
let%test _ = est_compatible Int Bool = false
let%test _ = est_compatible Int Rat = false


let est_compatible_list lt1 lt2 =
  try
    List.for_all2 est_compatible lt1 lt2
  with Invalid_argument _ -> false

let%test _ = est_compatible_list [] [] = true
let%test _ = est_compatible_list [Int] [Int] = true
let%test _ = est_compatible_list [Int; Rat] [Int; Int] = false
let%test _ = est_compatible_list [Int; Rat] [Int; Rat] = true

let getTaille t =
  match t with
  | Int -> 1
  | Bool -> 1
  | Rat -> 2
  | Undefined -> 0
  | Pointeur _ -> 1
  | Enum _ -> 1

let%test _ = List.map getTaille [Int; Bool; Rat; Undefined; Pointeur Int; Pointeur Rat; Enum "Mois"] = [1; 1; 2; 0; 1; 1; 1]
  
