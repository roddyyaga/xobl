open Patche

let s =
  Xml.(dtd >>& el_start "xml" &>> el_end)