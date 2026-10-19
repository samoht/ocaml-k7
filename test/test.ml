let () =
  Alcotest.run "vhs"
    (Test_vhs.[ suite; cram_suite; run_suite ]
    @ [ Test_eio.suite; Test_cli.suite ])
