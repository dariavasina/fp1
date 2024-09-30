open OUnit2
open Task21

let test_sum_of_divisors _ =
  assert_equal 0 (sum_of_divisors 1 0);
  assert_equal 1 (sum_of_divisors 1 1);
  assert_equal 3 (sum_of_divisors 6 3); 
  assert_equal 15 (sum_of_divisors 28 14)
  
let test_sum_amicable_numbers_recursive _ =
  assert_equal 0 (sum_amicable_numbers_recursive 1);
  assert_equal 0 (sum_amicable_numbers_recursive 2);
  assert_equal 0 (sum_amicable_numbers_recursive 3);
  assert_equal 0 (sum_amicable_numbers_recursive 5);
  assert_equal 220 (sum_amicable_numbers_recursive 300); 
  assert_equal 0 (sum_amicable_numbers_recursive 5)

let test_sum_amicable_numbers_tail_recursive _ =
  assert_equal 0 (sum_amicable_numbers_tail_recursive 1);
  assert_equal 0 (sum_amicable_numbers_tail_recursive 2);
  assert_equal 0 (sum_amicable_numbers_tail_recursive 3);
  assert_equal 0 (sum_amicable_numbers_tail_recursive 5);
  assert_equal 220 (sum_amicable_numbers_tail_recursive 300)

let test_sum_amicable_numbers_modular _ =
  assert_equal 0 (sum_amicable_numbers_modular 1);
  assert_equal 0 (sum_amicable_numbers_modular 2);
  assert_equal 0 (sum_amicable_numbers_modular 3);
  assert_equal 0 (sum_amicable_numbers_modular 5);
  assert_equal 220 (sum_amicable_numbers_modular 300)

let test_sum_amicable_numbers_map _ =
  assert_equal 0 (sum_amicable_numbers_map 1);
  assert_equal 0 (sum_amicable_numbers_map 2);
  assert_equal 0 (sum_amicable_numbers_map 3);
  assert_equal 0 (sum_amicable_numbers_map 5);
  assert_equal 220 (sum_amicable_numbers_map 300)

let test_sum_amicable_numbers_loop _ =
  assert_equal 0 (sum_amicable_numbers_loop 1);
  assert_equal 0 (sum_amicable_numbers_loop 2);
  assert_equal 0 (sum_amicable_numbers_loop 3);
  assert_equal 0 (sum_amicable_numbers_loop 5);
  assert_equal 220 (sum_amicable_numbers_loop 300)

let test_sum_amicable_numbers_seq _ =
  assert_equal 0 (sum_amicable_numbers_seq 1);
  assert_equal 0 (sum_amicable_numbers_seq 2);
  assert_equal 0 (sum_amicable_numbers_seq 3);
  assert_equal 0 (sum_amicable_numbers_seq 5);
  assert_equal 220 (sum_amicable_numbers_seq 300)

let suite =
  "TestTask21" >:::
  [
    "test_sum_of_divisors" >:: test_sum_of_divisors;
    "test_sum_amicable_numbers_recursive" >:: test_sum_amicable_numbers_recursive;
    "test_sum_amicable_numbers_tail_recursive" >:: test_sum_amicable_numbers_tail_recursive;
    "test_sum_amicable_numbers_modular" >:: test_sum_amicable_numbers_modular;
    "test_sum_amicable_numbers_map" >:: test_sum_amicable_numbers_map;
    "test_sum_amicable_numbers_loop" >:: test_sum_amicable_numbers_loop;
    "test_sum_amicable_numbers_seq" >:: test_sum_amicable_numbers_seq;
  ]
