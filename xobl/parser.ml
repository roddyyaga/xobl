open Patche
open Patche.Xml

type toplevel = [`Import of string | `Xidtype of string]

let import =
  el_start_empty "import" >>& data &>> el_end
  |> pipe (fun i -> `Import i)

let xidtype =
  el_start "xidtype" Attr.(return (str "name")) &>> el_end
  |> pipe (fun name -> `Xidtype name)

let x =
  dtd >>& el_start_empty "xcb" >>& many (import <|> xidtype) &>> el_end
