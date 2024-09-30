let string_to_digits s = 
  List.init (String.length s) (fun i -> int_of_char s.[i] - int_of_char '0')

let rec take n lst =
  match lst, n with
  | [], _ | _, 0 -> []
  | hd :: tl, n -> hd :: take (n - 1) tl


let rec sublist start len lst =
  match lst with
  | [] -> []
  | _ when start > 0 -> sublist (start - 1) len (List.tl lst)
  | hd :: tl when len > 0 -> hd :: sublist 0 (len - 1) tl
  | _ -> []


let rec max_product_recursive lst len =
  match lst with
  | [] -> 0
  | _ when List.length lst < len -> 0
  | _ -> 
      let product = List.fold_left ( * ) 1 (take len lst) in
      max product (max_product_recursive (List.tl lst) len)


let max_product_tail_recursive lst len =
  let rec aux lst len max_prod =
    match lst with
    | [] -> max_prod
    | _ when List.length lst < len -> max_prod
    | _ ->
        let current_product = List.fold_left ( * ) 1 (take len lst) in
        let new_max = max current_product max_prod in
        aux (List.tl lst) len new_max
  in
  aux lst len 0

let max_product_modular lst len =
  let sequences = List.filter (fun seq -> List.length seq = len) 
                   (List.init (List.length lst - len + 1) (fun i -> sublist i len lst)) in
  List.fold_left (fun max_prod seq -> max max_prod (List.fold_left ( * ) 1 seq)) 0 sequences

let max_product_with_map lst len =
  List.map (fun i -> List.fold_left ( * ) 1 (sublist i len lst)) (List.init (List.length lst - len + 1) (fun i -> i))
  |> List.fold_left max 0

(* let max_product_lazy lst len =
  let seq = Seq.init (List.length lst - len + 1) (fun i -> sublist i len lst) in
  Seq.fold_left (fun max_prod seq -> max max_prod (List.fold_left ( * ) 1 seq)) 0 seq *)
