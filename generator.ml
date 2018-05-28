open Util


let resolve_extension_path name =
  Filename.concat "xproto/src" (name ^ ".xml")


let load_extension file_name : Parser.protocol_file =
  resolve_extension_path file_name
  |> Parser.parse_file


let snake_cased name =
  if String.uppercase_ascii name = name then
    (* The string is already snake_cased, just make sure it's lowercase. *)
    String.lowercase_ascii name
  else
    (* The string is CamelCased. *)
    let buf = Buffer.create 16 in
    StringLabels.iteri name ~f:(fun i -> function
      | c when i = 0 ->
        Buffer.add_char buf (Char.lowercase_ascii c)
      | 'A' .. 'Z' as c ->
        (* We want to make sure something like GLXContext is turned into
           glxcontext and not g_l_x_context.
           We can't turn it into glx_context because then DECnet would become
           de_cnet. *)
        let prev = name.[i - 1] in
        if Char.lowercase_ascii prev = prev then (
          Buffer.add_char buf '_';
          Buffer.add_char buf (Char.lowercase_ascii c)
        ) else
          Buffer.add_char buf (Char.lowercase_ascii c)
      | c ->
        Buffer.add_char buf c
    );
    Buffer.contents buf


let id_str = function
  | Analyzer.Id n -> snake_cased n
  | Analyzer.Ext_id (e, n) ->
    String.capitalize_ascii (snake_cased e) ^ "." ^ snake_cased n


let prim_to_string =
  let open Analyzer.Prim in function
    | Void   -> "unit"
    | Char   -> "char"
    | Byte   -> "char"
    | Bool   -> "bool"
    | Int8   -> "int"
    | Int16  -> "int"
    | Int32  -> "int32"
    | Fd     -> "int"
    | Card8  -> "int"
    | Card16 -> "int"
    | Card32 -> "int32"
    | Card64 -> "int64"
    | Float  -> "float"
    | Double -> "float"
    | Xid    -> "xid"


let x_type_str = function
  | Analyzer.Prim t -> prim_to_string t
  | Analyzer.Ref id -> id_str id


(* To be usable these extensions certainly need some more work; maybe we should
   define a thin wrapper and not do any complex usage tracking stuff here e.g.
   deciding whether types should be opaque and so on. *)

let generate out (ext : Analyzer.Pass_2.extension_p2) =
  let o fmt = Printf.fprintf out fmt in
  ext.version |> Option.iter (fun (maj, min) ->
    o "let version = (%d, %d)\n" maj min);
  ext.query_name |> Option.iter (fun n ->
    o "let query_extension_name = %S\n" n);
  ext.declarations |> List.iter begin function
    | `Alias (n, t) ->
      (* Should the types be opaque?
        If yes, what are the implications of that?
        Do we have to track usage to know whether they should ever be supplied
        by the users or can we get by just using the enums?
      *)
      o "type %s = %s\n" (snake_cased n) (x_type_str t)

    | `X_id_union (n, _) ->
      (* XID unions are hardly used in the codebase, and outputting a variant
         type rather than simply aliasing them to XIDs would be way more
         trouble than it's worth. This might change in the future, but for now
         it's good enough. *)
      o "type %s = xid\n" (snake_cased n)

    | `Enum (name, items) ->
      (* We need to know a few things here:
        - output an enumeration? a bitmask? both? (solved, sort of)
        - which types do we need conversion functions to and from for?
        - for bitmasks, do we need to output conversion functions to the "val"
          items too? (probably not, but maybe we could provide some compare
          functions)
      *)
      ()

    | _ ->
      ()
  end



let%test_unit "analyzer test" =
  let files =
    [ "bigreq"; "composite"; "damage"; "dpms"; "dri2"; "dri3"; "ge"; "glx"
    ; "present"; "randr"; "record"; "render"; "res"; "screensaver"; "shape"
    ; "shm"; "sync"; "xc_misc"; "xevie"; "xf86dri"; "xf86vidmode"; "xfixes"
    ; "xinerama"; "xinput"; "xkb"; "xprint"; "xproto"; "xselinux"; "xtest"
    ; "xvmc"; "xv" ]
  in
  let exts = List.map load_extension files in
  let exts = Analyzer.analyze_extensions exts in
  List.iter (generate stdout) exts
