open OUnit2
open Task21
  
let test_sum_amicable_numbers_recursive _ =
  assert_equal 0 (sum_amicable_numbers_recursive 100);
  assert_equal 0 (sum_amicable_numbers_recursive 200);
  assert_equal 504 (sum_amicable_numbers_recursive 300);
  assert_equal 504 (sum_amicable_numbers_recursive 500);
  assert_equal 504 (sum_amicable_numbers_recursive 1000)


let test_sum_amicable_numbers_tail_recursive _ =
  assert_equal 0 (sum_amicable_numbers_tail_recursive 100);
  assert_equal 0 (sum_amicable_numbers_tail_recursive 200);
  assert_equal 504 (sum_amicable_numbers_tail_recursive 300);
  assert_equal 504 (sum_amicable_numbers_tail_recursive 500);
  assert_equal 504 (sum_amicable_numbers_tail_recursive 1000)

let test_sum_amicable_numbers_modular _ =
  assert_equal 0 (sum_amicable_numbers_modular 100);
  assert_equal 0 (sum_amicable_numbers_modular 200);
  assert_equal 504 (sum_amicable_numbers_modular 300);
  assert_equal 504 (sum_amicable_numbers_modular 500);
  assert_equal 504 (sum_amicable_numbers_modular 1000)

let test_sum_amicable_numbers_map _ =
  assert_equal 0 (sum_amicable_numbers_map 100);
  assert_equal 0 (sum_amicable_numbers_map 200);
  assert_equal 504 (sum_amicable_numbers_map 300);
  assert_equal 504 (sum_amicable_numbers_map 500);
  assert_equal 504 (sum_amicable_numbers_map 1000)

let test_sum_amicable_numbers_loop _ =
  assert_equal 0 (sum_amicable_numbers_loop 100);
  assert_equal 0 (sum_amicable_numbers_loop 200);
  assert_equal 504 (sum_amicable_numbers_loop 300);
  assert_equal 504 (sum_amicable_numbers_loop 500);
  assert_equal 504 (sum_amicable_numbers_loop 1000)

let test_sum_amicable_numbers_seq _ =
  assert_equal 0 (sum_amicable_numbers_seq 100);
  assert_equal 0 (sum_amicable_numbers_seq 200);
  assert_equal 504 (sum_amicable_numbers_seq 300);
  assert_equal 504 (sum_amicable_numbers_seq 500);
  assert_equal 504 (sum_amicable_numbers_seq 1000)

let suite =
  "TestTask21" >:::
  [
    "test_sum_amicable_numbers_recursive" >:: test_sum_amicable_numbers_recursive;
    "test_sum_amicable_numbers_tail_recursive" >:: test_sum_amicable_numbers_tail_recursive;
    "test_sum_amicable_numbers_modular" >:: test_sum_amicable_numbers_modular;
    "test_sum_amicable_numbers_map" >:: test_sum_amicable_numbers_map;
    "test_sum_amicable_numbers_loop" >:: test_sum_amicable_numbers_loop;
    "test_sum_amicable_numbers_seq" >:: test_sum_amicable_numbers_seq;
  ]
