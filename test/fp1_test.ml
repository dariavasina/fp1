open OUnit2


let () =
  let suite = 
    "MainTestSuite" >:::
    [
      Test_task21.suite;  
      Test_task8.suite;  
    ]
  in
  run_test_tt_main suite