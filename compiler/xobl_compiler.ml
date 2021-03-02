module Parser = struct
  type error = Parser_utils.error

  let parse source =
    Xmlm.make_input ~strip:true source
    |> Patche.Xml.make_input |> Patche.Xml.run Parser.xcb
end

let elaborate = Elaborate_unions_to_switch.unions_to_switch

module Parsetree = Parsetree
