let rec sum_of_divisors n i = 
  match i with
  | 0 -> 0
  | _ when n mod i = 0 -> i + sum_of_divisors n (i - 1)
  | _ -> sum_of_divisors n (i - 1)

let sum_amicable_numbers_recursive limit =
  let rec aux n =
    match n with
    | 0 -> 0
    | _ ->
      let a = sum_of_divisors n (n / 2) in
      let b = sum_of_divisors a (a / 2) in
      match (a <> n, b = n) with
      | (true, true) -> n + aux (n - 1)
      | _ -> aux (n - 1)
  in aux (limit - 1)

let rec sum_of_divisors_tail n i acc =
  match i with
  | 0 -> acc
  | _ when n mod i = 0 -> sum_of_divisors_tail n (i - 1) (acc + i)
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

let sum_of_divisors_modular n =
  List.fold_left (+) 0 (List.filter (fun i -> n mod i = 0) (List.init (n / 2) (fun i -> i + 1)))

let is_amicable a =
  let b = sum_of_divisors_modular a in
  b <> a && sum_of_divisors_modular b = a

let find_amicable_numbers limit =
  List.filter is_amicable (List.init limit (fun i -> i))

let sum_amicable_numbers_modular limit =
  List.fold_left (+) 0 (find_amicable_numbers limit)


let sum_of_divisors_map n =
  List.fold_left (+) 0 (List.filter (fun i -> n mod i = 0) (List.init (n / 2) (fun i -> i + 1)))

let is_amicable_map a =
  let b = sum_of_divisors_map a in
  b <> a && sum_of_divisors_map b = a

let sum_amicable_numbers_map limit =
  List.fold_left (+) 0 (List.map (fun n -> if is_amicable n then n else 0) (List.init limit (fun i -> i)))

(* Функция для вычисления суммы делителей числа n *)
let sum_of_divisors_loop n =
  let sum = ref 0 in
  for i = 1 to n / 2 do
    if n mod i = 0 then sum := !sum + i
  done;
  !sum

(* Функция для проверки, являются ли два числа дружественными *)
let is_amicable_loop a =
  let b = sum_of_divisors_loop a in
  b <> a && sum_of_divisors_loop b = a

(* Функция для нахождения суммы всех дружественных чисел ниже заданного лимита *)
let sum_amicable_numbers_loop limit =
  let sum = ref 0 in
  for n = 1 to limit do
    if is_amicable_loop n then sum := !sum + n
  done;
  !sum
  
