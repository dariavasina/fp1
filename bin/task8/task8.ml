open Base

let string_to_digits s = 
  List.init (String.length s) ~f:(fun i -> Char.to_int s.[i] - Char.to_int '0')


let rec take n lst =
  match lst, n with
  | [], _ | _, 0 -> []
  | hd :: tl, n -> hd :: take (n - 1) tl


let rec sublist start len lst =
  match lst with
  | [] -> []
  | _ :: tl when start > 0 -> sublist (start - 1) len tl
  | hd :: tl -> 
      if len > 0 then 
        hd :: sublist 0 (len - 1) tl 
      else 
        []
  
let rec max_product_recursive lst len =
  match lst with
  | [] -> 0
  | _ when List.length lst < len -> 0
  | _ :: xs -> 
      let product = List.fold_left ~f:( * ) ~init:1 (take len lst) in
      max product (max_product_recursive xs len)

let max_product_tail_recursive lst len =
  let rec aux lst len max_prod =
    match lst with
    | [] -> max_prod
    | _ when List.length lst < len -> max_prod
    | _ :: xs ->
        let current_product = List.fold_left ~f:( * ) ~init:1 (take len lst) in
        let new_max = max current_product max_prod in
        aux xs len new_max
  in
  aux lst len 0

let max_product_modular lst len =
  match lst with
  | _ when List.length lst < len -> 0
  | _ ->
    let sequences = List.init (List.length lst - len + 1) ~f:(fun i -> sublist i len lst) in
    List.fold_left ~f:(fun max_prod seq -> max max_prod (List.fold_left ~f:( * ) ~init:1 seq)) ~init:0 sequences


let max_product_with_map lst len =
  match lst with
  | _ when List.length lst < len -> 0
  | _ ->
    List.map ~f:(fun i -> List.fold_left ~f:( * ) ~init:1 (sublist i len lst)) 
      (List.init (List.length lst - len + 1) ~f:(fun i -> i))
    |> List.fold_left ~f:max ~init:0


let max_product_seq lst len =
  match lst with
  | _ when List.length lst < len -> 0
  | _ ->
    let seq = Stdlib.Seq.init (List.length lst - len + 1) (fun i -> sublist i len lst) in
    Stdlib.Seq.fold_left (fun max_prod seq -> max max_prod (List.fold_left ~f:( * ) ~init:1 seq)) 0 seq
  