open Xobl_parser2
open OUnit2


let inp str =
  Xmlm.make_input ~strip:true (`String (0, str))
  |> Lazy_list.of_xml_input


let test_parse _ =
  let i = inp {xml|
<xcb
  header="xkb" extension-xname="XKEYBOARD" extension-name="xkb"
  major-version="1" minor-version="0"
>
  <import>ayy</import>
  <import>tfw</import>
  <xidtype name="xid" />
  <xidunion name="DRAWABLE">
    <type>WINDOW</type>
    <type>PIXMAP</type>
  </xidunion>
  <typedef oldname="CARD32" newname="BOOL32" />
  <eventcopy name="KeyRelease" number="3" ref="KeyPress" />
  <errorcopy name="Window" number="3" ref="Value" />
  <eventstruct name="EventForSend">
    <allowed extension="Input" xge="false" opcode-min="0" opcode-max="16" />
  </eventstruct>
  <enum name="DeviceUse">
    <item name="IsXPointer"> <value>0</value></item>
    <item name="IsXKeyboard"><value>1</value></item>
  </enum>
  <union name="Behavior">
    <field name="common" type="CommonBehavior" />
    <field name="match" type="CARD8" altenum="SymInterpretMatch" />
    <pad bytes="21" />
    <list type="VISUALTYPE" name="visuals">
      <fieldref>visuals_len</fieldref>
    </list>
  </union>
</xcb>
  |xml} in
  match Parser.x i with
  | Error err ->
    assert_failure err
  | Ok (res, _) ->
    res |> assert_equal (Parser.Extension (
      { name = "xkb"; file_name = "xkb"; query_name = "XKEYBOARD"
      ; multiword = false; version = (1, 0) },
      [ `Import "ayy"
      ; `Import "tfw"
      ; `Xidtype "xid"
      ; `Xidunion ("DRAWABLE", ["WINDOW"; "PIXMAP"])
      ; `Typedef ("BOOL32", "CARD32")
      ; `Eventcopy ("KeyRelease", 3, "KeyPress")
      ; `Errorcopy ("Window", 3, "Value")
      ; `Eventstruct ("EventForSend", ["Input", false, (0, 16)])
      ; `Enum ("DeviceUse", [("IsXPointer", 0L); ("IsXKeyboard", 1L)])
      ; `Union ("Behavior", [
          `Field ("common", "CommonBehavior", None);
          `Field ("match", "CARD8", Some (`Altenum "SymInterpretMatch"));
          `Pad (false, `Bytes 21);
          `List ("visuals", "VISUALTYPE", None, `Fieldref "visuals_len");
        ])
      ]))


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
