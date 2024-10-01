open Base

let rec sum_of_divisors_recursive n i = 
  match i with
  | 0 -> 0
  | _ when n % i = 0 -> i + sum_of_divisors_recursive n (i - 1)
  | _ -> sum_of_divisors_recursive n (i - 1)

let sum_amicable_numbers_recursive limit =
  let rec aux n =
    match n with
    | 0 -> 0
    | _ ->
      let a = sum_of_divisors_recursive n (n / 2) in
      let b = sum_of_divisors_recursive a (a / 2) in
      match (a <> n, b = n) with
      | (true, true) -> n + aux (n - 1)
      | _ -> aux (n - 1)
  in aux (limit - 1)

let rec sum_of_divisors_tail n i acc =
  match i with
  | 0 -> acc
  | _ when n % i = 0 -> sum_of_divisors_tail n (i - 1) (acc + i)
  | _ -> sum_of_divisors_tail n (i - 1) acc

let sum_amicable_numbers_tail_recursive limit =
  let rec aux n acc =
    match n with
    | 0 -> acc
    | _ ->
      let a = sum_of_divisors_tail n (n / 2) 0 in
      let b = sum_of_divisors_tail a (a / 2) 0 in
      (* check if n and a are amicable *)
      match (a <> n, b = n) with 
      | (true, true) -> aux (n - 1) (acc + n)
      | _ -> aux (n - 1) acc
  in aux (limit - 1) 0

let sum_of_divisors n =
  List.fold_left 
    ~f:(fun acc i -> 
      match n % i with
      | 0 -> acc + i  
      | _ -> acc)
    ~init:0
    (List.init (n / 2) ~f:(fun i -> i + 1)) 

let is_amicable a =
  let b = sum_of_divisors a in
  b <> a && sum_of_divisors b = a

let sum_amicable_numbers_modular limit =
  List.fold_left
    ~f:(fun acc i ->
      match is_amicable i with
      | true -> acc + i  
      | _ -> acc) 
    ~init:0
    (List.init limit ~f:(fun i -> i))

let sum_amicable_numbers_map limit =
  List.init limit ~f:(fun i -> i)
  |> List.map ~f:(fun x ->
        let b = sum_of_divisors x in
        match (b <> x, sum_of_divisors b = x) with
        | (true, true) -> x
        | _ -> 0
      )
  |> List.fold_left ~f:(+) ~init:0
    
let sum_amicable_numbers_loop limit =
  let sum_of_divisors_loop n =
    let sum = ref 0 in
    for i = 1 to n / 2 do
      match n % i with
      | 0 -> sum := !sum + i  
      | _ -> () 
    done; 
    !sum 
  in
  let is_amicable a =
    let b = sum_of_divisors_loop a in
    b <> a && sum_of_divisors_loop b = a
  in
  let sum = ref 0 in
  for n = 1 to limit do
    match is_amicable n with
    | true -> sum := !sum + n  
    | _ -> ()
  done;
  !sum

let sum_amicable_numbers_seq limit =
  let seq = Stdlib.Seq.unfold (fun x -> Some (x, x + 1)) 1 in
  
  let amicable_numbers =
    Stdlib.Seq.filter (fun n -> is_amicable n) (Stdlib.Seq.take_while (fun n -> n < limit) seq)
  in
  Stdlib.Seq.fold_left (+) 0 amicable_numbers
