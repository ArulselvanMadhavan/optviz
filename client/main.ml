let () =
  let open Js_of_ocaml in
  let s = Js.string "Hello from OCaml!" in
  Firebug.console##log s
;;
