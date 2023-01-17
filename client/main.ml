open! Base
open! Bonsai
open! Bonsai_web
open! Async_kernel
open! Async_js
open! Js_of_ocaml

module M = struct
  type t =
    { error : Error.t option
    ; spec : string option
    ; images : string list
    ; show_images : bool
    }
  [@@deriving sexp, equal]

  let default = { error = None; spec = None; images = []; show_images = false }
end

module V = struct
  type t =
    | Opt125m_output_range
    | Opt_all_output_range
    | Opt125m_heatmap_sensitive_layers of string list
  [@@deriving typed_variants, sexp, equal]
end

module Form = Bonsai_web_ui_form

module Action = struct
  type t =
    | Spec of string option
    | Images of string list
    | Error of Error.t option
  (* JSOO - vegaEmbed *)
  (* | Image *)
  [@@deriving sexp_of]
end

let handle_spec_change s =
  let json_spec =
    Js.Unsafe.fun_call
      (Js.Unsafe.js_expr "JSON.parse")
      [| Js.Unsafe.inject (Js.string s) |]
  in
  let _ =
    Js.Unsafe.fun_call
      (Js.Unsafe.js_expr "vegaEmbed")
      [| Js.Unsafe.inject (Js.string "#viz"); json_spec |]
  in
  ()
;;

let fetch_spec inject s =
  let open Effect.Let_syntax in
  let%bind response =
    Effect.of_deferred_fun
      (fun p -> Async_js.Http.get ~arguments:[] p)
      ("/recipe/" ^ s ^ ".vg.json")
  in
  if Core.Or_error.is_error response
  then inject (Action.Error (Core.Result.error response))
  else (
    let spec = Core.Or_error.ok_exn response in
    handle_spec_change spec;
    inject (Action.Spec (Some spec)))
;;

let form_of_v (_inject : (Action.t -> unit Effect.t) Value.t) : V.t Form.t Computation.t =
  Form.Typed.Variant.make
    (module struct
      module Typed_variant = V.Typed_variant

      let form_for_variant : type a. a Typed_variant.t -> a Form.t Computation.t
        = function
        | Opt125m_output_range -> Bonsai.const (Form.return ())
        | Opt_all_output_range -> Bonsai.const (Form.return ())
        | Opt125m_heatmap_sensitive_layers ->
          Form.Elements.Multiselect.list
            [%here]
            (module String)
            (Value.return [
                "model.decoder.layers.1.fc1";"model.decoder.layers.8.self_attn.v_proj";"model.decoder.layers.9.self_attn.v_proj";"model.decoder.layers.7.self_attn.v_proj";"model.decoder.layers.11.self_attn.v_proj";"model.decoder.layers.4.self_attn.v_proj";"model.decoder.layers.10.self_attn.v_proj";"model.decoder.layers.6.self_attn.v_proj";"model.decoder.layers.2.fc1";"model.decoder.layers.1.fc2"
              ])
      ;;
    end)
;;

let handle_v_change inject = function
  | V.Opt125m_output_range ->
    fetch_spec
      inject
      (Base.String.lowercase (Sexp.to_string (V.sexp_of_t V.Opt125m_output_range)))
  | V.Opt_all_output_range ->
    fetch_spec
      inject
      (Base.String.lowercase (Sexp.to_string (V.sexp_of_t V.Opt_all_output_range)))
  | V.Opt125m_heatmap_sensitive_layers selected ->
    handle_spec_change "{}";
    inject (Images selected)
;;

let build_image_path layer_name = "data/opt125m/heatmap/images/" ^ layer_name ^ ".png"

let view_of_form : Vdom.Node.t Computation.t =
  let open! Bonsai.Let_syntax in
  let%sub state, inject =
    Bonsai.state_machine0
      [%here]
      (module M)
      (module Action)
      ~default_model:M.default
      ~apply_action:(fun ~inject:_ ~schedule_event:_ model action ->
        match action with
        | Spec s -> { model with spec = s }
        | Images xs -> { model with images = xs; show_images = true }
        | Error e -> { model with error = e })
  in
  let%sub form_v = form_of_v inject in
  let%sub () =
    Form.Dynamic.on_change
      (module V)
      form_v
      ~f:(Value.map inject ~f:(fun inject -> handle_v_change inject))
  in
  let%arr form_v = form_v
  and state = state in
  (* and inject = inject in *)
  let value = Form.value form_v in
  let images =
    if state.show_images
    then
      List.map state.images ~f:(fun layer_name ->
        Vdom.Node.create
          "img"
          ~attr:(Vdom.Attr.many [ Vdom.Attr.src (build_image_path layer_name) ])
          [])
    else []
  in
  Vdom.Node.div
    ([ Form.view_as_vdom form_v
     ; Vdom.Node.sexp_for_debugging ([%sexp_of: V.t Or_error.t] value)
       (* ; Vdom.Node.text (Sexp.to_string (M.sexp_of_t state)) *)
     ]
    @ images)
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" view_of_form
;;
