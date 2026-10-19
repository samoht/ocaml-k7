let () =
  Alcotest.run "vhs" [ Test_value.suite; Test_vhs.suite; Test_cram.suite ]
