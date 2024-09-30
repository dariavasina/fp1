open OUnit2
open Task8 

let test_string_to_digits _ =
  assert_equal [1; 2; 3] (string_to_digits "123");
  assert_equal [4; 5; 6; 7; 8; 9; 0] (string_to_digits "4567890");
  assert_equal [] (string_to_digits "");
  assert_equal [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] (string_to_digits "0123456789")

let test_take _ =
  assert_equal [1; 2] (take 2 [1; 2; 3; 4]);
  assert_equal [1; 2; 3] (take 3 [1; 2; 3]);
  assert_equal [] (take 0 [1; 2; 3]);
  assert_equal [] (take 5 [1; 2; 3]);
  assert_equal [1] (take 1 [1])

let test_sublist _ =
  assert_equal [2; 3] (sublist 1 2 [1; 2; 3; 4]);
  assert_equal [1; 2] (sublist 0 2 [1; 2; 3; 4]);
  assert_equal [] (sublist 0 0 [1; 2; 3; 4]);
  assert_equal [] (sublist 5 2 [1; 2; 3; 4]);
  assert_equal [] (sublist 2 0 [1; 2; 3; 4])

let test_max_product_recursive _ =
  assert_equal 12 (max_product_recursive [1; 2; 3; 4] 2);
  assert_equal 24 (max_product_recursive [1; 2; 3; 4] 3); 
  assert_equal 0 (max_product_recursive [] 2);
  assert_equal 0 (max_product_recursive [1] 2);
  assert_equal 0 (max_product_recursive [1; 2] 3)

let test_max_product_tail_recursive _ =
  assert_equal 12 (max_product_tail_recursive [1; 2; 3; 4] 2);  
  assert_equal 24 (max_product_tail_recursive [1; 2; 3; 4] 3); 
  assert_equal 0 (max_product_tail_recursive [] 2);
  assert_equal 0 (max_product_tail_recursive [1] 2);
  assert_equal 0 (max_product_tail_recursive [1; 2] 3)

let test_max_product_modular _ =
  assert_equal 12 (max_product_modular [1; 2; 3; 4] 2); 
  assert_equal 24 (max_product_modular [1; 2; 3; 4] 3); 
  assert_equal 0 (max_product_modular [] 2);
  assert_equal 0 (max_product_modular [1] 2);
  assert_equal 0 (max_product_modular [1; 2] 3)

let test_max_product_with_map _ =
  assert_equal 12 (max_product_with_map [1; 2; 3; 4] 2);  
  assert_equal 24 (max_product_with_map [1; 2; 3; 4] 3); 
  assert_equal 0 (max_product_with_map [] 2);
  assert_equal 0 (max_product_with_map [1] 2);
  assert_equal 0 (max_product_with_map [1; 2] 3)

let test_max_product_lazy _ =
  assert_equal 12 (max_product_lazy [1; 2; 3; 4] 2); 
  assert_equal 24 (max_product_lazy [1; 2; 3; 4] 3);
  assert_equal 0 (max_product_lazy [] 2);
  assert_equal 0 (max_product_lazy [1] 2);
  assert_equal 0 (max_product_lazy [1; 2] 3)

let suite =
  "MaxProductTests" >:::
  [
    "test_string_to_digits" >:: test_string_to_digits;
    "test_take" >:: test_take;
    "test_sublist" >:: test_sublist;
    "test_max_product_recursive" >:: test_max_product_recursive;
    "test_max_product_tail_recursive" >:: test_max_product_tail_recursive;
    "test_max_product_modular" >:: test_max_product_modular;
    "test_max_product_with_map" >:: test_max_product_with_map;
    "test_max_product_lazy" >:: test_max_product_lazy;
  ]
