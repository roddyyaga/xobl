type attr = string * string

type el =
  string * attr list * t list

and t =
  | E of el
  | D of string

let parse_file fname =
  CCIO.with_in fname (fun inp ->
    let xml_inp = Xmlm.make_input ~strip:true (`Channel inp) in
    let el ((_, name), attrs) childs =
      E (name, (List.map (fun ((_, n), v) -> (n, v)) attrs), childs)
    in
    let data str = D str in
    let (_, x) = Xmlm.input_doc_tree ~el ~data xml_inp in
    x
  )
