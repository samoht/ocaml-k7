open Alcobar

let test_parse_no_exception src = ignore (Vhs.Cram.parse src)

let suite =
  ( "vhs",
    [ test_case "parse arbitrary bytes" [ bytes ] test_parse_no_exception ] )
