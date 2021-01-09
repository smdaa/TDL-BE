type typ = Bool | Int | Rat | Undefined | Pointeur of typ | Enum of string

let rec string_of_type t = 
  match t with
  | Bool ->  "Bool"
  | Int  ->  "Int"
  | Rat  ->  "Rat"
  | Undefined -> "Undefined"
  | Pointeur t -> "Pointeur sur " ^ (string_of_type t)
  | Enum t -> "Enumeration de " ^ t


let rec est_compatible t1 t2 =
  match t1, t2 with
  | Bool, Bool -> true
  | Int, Int -> true
  | Rat, Rat -> true
  | Undefined, Undefined -> true
  | Pointeur t1, Pointeur t2 -> est_compatible t1 t2
  | Enum en1, Enum en2 -> en1=en2
  | _ -> false 

let est_compatible_list lt1 lt2 =
  try
    List.for_all2 est_compatible lt1 lt2
  with Invalid_argument _ -> false

let getTaille t =
  match t with
  | Int -> 1
  | Bool -> 1
  | Rat -> 2
  | Undefined -> 0
  | Pointeur _ -> 1
  | Enum _ -> 1
  
