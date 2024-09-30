open Alcotest
open Task8 (* Подключаем модуль Task8, если функции в нем *)

(* Тест для функции string_to_digits *)
let test_string_to_digits () =
  let input = "12345" in
  let expected = [1; 2; 3; 4; 5] in
  let actual = string_to_digits input in
  check (list int) "same digits" expected actual

(* Тест для функции take *)
let test_take () =
  let input = [1; 2; 3; 4; 5] in
  let expected = [1; 2; 3] in
  let actual = take 3 input in
  check (list int) "take first 3 elements" expected actual

(* Тест для функции sublist *)
let test_sublist () =
  let input = [1; 2; 3; 4; 5] in
  let expected = [3; 4] in
  let actual = sublist 2 2 input in
  check (list int) "sublist from index 2, length 2" expected actual

(* Тест для функции max_product_recursive *)
let test_max_product_recursive () =
  let input = [1; 2; 3; 4; 5] in
  let expected = 60 in
  let actual = max_product_recursive input 3 in
  check int "max product recursive of 3 digits" expected actual

(* Тест для функции max_product_tail_recursive *)
let test_max_product_tail_recursive () =
  let input = [1; 2; 3; 4; 5] in
  let expected = 60 in
  let actual = max_product_tail_recursive input 3 in
  check int "max product tail recursive of 3 digits" expected actual

(* Тест для функции max_product_modular *)
let test_max_product_modular () =
  let input = [1; 2; 3; 4; 5] in
  let expected = 60 in
  let actual = max_product_modular input 3 in
  check int "max product modular of 3 digits" expected actual

(* Тест для функции max_product_with_map *)
let test_max_product_with_map () =
  let input = [1; 2; 3; 4; 5] in
  let expected = 60 in
  let actual = max_product_with_map input 3 in
  check int "max product with map of 3 digits" expected actual

(* Тест для функции max_product_lazy
let test_max_product_lazy () =
  let input = [1; 2; 3; 4; 5] in
  let expected = 60 in
  let actual = max_product_lazy input 3 in
  check int "max product lazy of 3 digits" expected actual *)

(* Запуск всех тестов *)
let () =
  run "Task8 Tests" [
    "string_to_digits", [ test_case "string_to_digits" `Quick test_string_to_digits ];
    "take", [ test_case "take" `Quick test_take ];
    "sublist", [ test_case "sublist" `Quick test_sublist ];
    "max_product_recursive", [ test_case "max_product_recursive" `Quick test_max_product_recursive ];
    "max_product_tail_recursive", [ test_case "max_product_tail_recursive" `Quick test_max_product_tail_recursive ];
    "max_product_modular", [ test_case "max_product_modular" `Quick test_max_product_modular ];
    "max_product_with_map", [ test_case "max_product_with_map" `Quick test_max_product_with_map ];
    (* "max_product_lazy", [ test_case "max_product_lazy" `Quick test_max_product_lazy ]; *)
  ]
