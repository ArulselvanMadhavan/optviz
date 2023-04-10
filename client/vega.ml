open! Js_of_ocaml

let vega_div = "#viz"

let json_parse s =
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "JSON.parse") [| Js.Unsafe.inject (Js.string s) |]
;;

let vega_obj_key = "VEGA_DEBUG"
let await = Fun.flip Fut.await

let vega_embed json_spec =
  let vpromise = Jv.call Jv.global "vegaEmbed" [| Jv.of_string vega_div; json_spec |] in
  let attach_to_global view = Jv.set Jv.global vega_obj_key view in
  Fut.of_promise ~ok:attach_to_global vpromise |> await Result.get_ok;
  Brr.Console.(log [ str "vega_debug attached" ])
;;

let build_record child parent elem rank color =
  Jv.obj
    [| "id", Jv.of_int child
     ; "parent", parent
     ; "name", elem
     ; "rank", Jv.of_int rank
     ; "color", Jv.of_string color
    |]
;;

let update_dataset ?(name = "main") values =
  let open Jv in
  let open Base in
  let values = Array.of_list values |> of_jv_array in
  let v = find global vega_obj_key |> Option.value_exn in
  let view = find v "view" |> Option.value_exn in
  let vega = find global "vega" |> Option.value_exn in
  let cset = call vega "changeset" [||] in
  let rm = call cset "remove" [| get vega "truthy" |] in
  let ins = call rm "insert" [| values |] in
  let change = call view "change" [| of_string name; ins |] in
  let _ = call change "run" [||] in
  ()
;;

let build_quantized_view fp_xs int_xs vsq_xs =
  let open Jv in
  let update_data xs val_func =
    List.iter
      (fun (type_, qvalues) ->
        let values =
          List.map
            (fun value -> obj [| "type_", of_string type_; "value", val_func value |])
            qvalues
        in
        update_dataset ~name:("data_" ^ type_) values)
      xs
  in
  update_data fp_xs of_float;
  update_data int_xs of_int;
  update_data vsq_xs (fun (x, _) -> of_int x)
;;
