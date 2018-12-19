open Xobl_parser
open Xobl_elaborate
open Xobl_ocaml_backend

module String_map = Types.String_map


let parse file_name =
  Parser.parse_file file_name
  |> P0_to_extension.lift_imports


let () =
  let exts =
    Sys.argv
    |> Array.to_list
    |> List.tl
    |> List.map parse
  in
  Cache.init exts;
  let map = String_map.empty in
  let exts =
    List.fold_left (fun map ext ->
      String_map.add map ~key:ext.Types.file_name ~data:ext
    ) map exts
  in
  let exts =
    exts
    |> P1_resolve.pass
    |> P2_fields.pass
  in
  exts |> String_map.iter ~f:(fun ~key ~data:ext ->
    CCIO.with_out (key ^ ".ml") (fun out ->
      Generator.generate exts out ext
    )
  )