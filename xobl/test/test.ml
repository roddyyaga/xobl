open Xobl_parser2
open OUnit2


let inp str = Lazy_list.of_xml_input (Xmlm.make_input ~strip:true (`String (0, str)))


let test _ctx =
  let i = inp {|
<xcb>
  <import>ayy</import>
  <import>tfw</import>
  <xidtype name="xid" />
</xcb>
|} in
  match Parser.x i with
  | Ok (res, _) ->
    assert_equal [`Import "ayy"; `Import "tfw"; `Xidtype "xid"] res
  | Error err ->
    assert_failure err


let suite =
  "eq tests" >:::
    [ "eq" >:: test ]


let () =
  run_test_tt_main suite