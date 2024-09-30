open OUnit2
open Task8 

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
  assert_equal 0 (max_product_with_map [1] 2)

let test_max_product_lazy _ =
  assert_equal 12 (max_product_lazy [1; 2; 3; 4] 2); 
  assert_equal 24 (max_product_lazy [1; 2; 3; 4] 3);
  assert_equal 0 (max_product_lazy [] 2);
  assert_equal 0 (max_product_lazy [1] 2);
  assert_equal 0 (max_product_lazy [1; 2] 3)

let suite =
  "MaxProductTests" >:::
  [
    "test_max_product_recursive" >:: test_max_product_recursive;
    "test_max_product_tail_recursive" >:: test_max_product_tail_recursive;
    "test_max_product_modular" >:: test_max_product_modular;
    "test_max_product_with_map" >:: test_max_product_with_map;
    "test_max_product_lazy" >:: test_max_product_lazy;
  ]
