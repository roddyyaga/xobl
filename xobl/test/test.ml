open Xobl_parser2
open OUnit2


let inp str =
  Xmlm.make_input ~strip:true (`String (0, str))
  |> Lazy_list.of_xml_input


let test_parse _ =
  let i = inp {xml|
<xcb>
  <import>ayy</import>
  <import>tfw</import>
  <xidtype name="xid" />
</xcb>
  |xml} in
  match Parser.x i with
  | Ok (res, _) ->
    assert_equal [`Import "ayy"; `Import "tfw"; `Xidtype "xid"] res
  | Error err ->
    assert_failure err


let test_attr _ =
  let i = [("", "boku_no"), "chateau"; ("", "num"), "1"] in
  let parser =
    Attr.tuple4
      (Attr.int "num")
      (Attr.bool_t "mmbk")
      (Attr.str "boku_no")
      (Attr.str_o "yunomi")
  in
  match parser i with
  | Ok x -> assert_equal (1, true, "chateau", None) x
  | Error err -> assert_failure err


let () =
  run_test_tt_main
    ("tests" >:::
      [ "parse full" >:: test_parse
      ; "parse attrs" >:: test_attr ])