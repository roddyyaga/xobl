open Patche
open Patche.Xml

type toplevel = [`Import of string | `Xidtype of string]

let import =
  el_start "import" >>& data &>> el_end
  |> pipe (fun i -> `Import i)

let xidtype =
  el_start "xidtype" &>> el_end
  |> pipe_result (function
    | [("", "name"), name] -> Ok (`Xidtype name)
    | _ -> Error "expected name=\"<name>\""
  )

let x =
  dtd >>& el_start "xcb" >>& many (import <|> xidtype) &>> el_end
