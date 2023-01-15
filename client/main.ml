open! Base
open! Bonsai
open! Bonsai_web
open! Async_kernel
open! Async_js

module M = struct
  type t =
    { error : Error.t option
    ; spec : string option
    ; images : string list option
    }
  [@@deriving sexp, equal]

  let default = { error = None; spec = None; images = None }
end

module V = struct
  type t =
    | Opt125m_output_range
    | B of string
    | C of string
  [@@deriving typed_variants, sexp, equal]
end

module Form = Bonsai_web_ui_form

module Action = struct
  type t = Spec of string
  (* JSOO - vegaEmbed *)
  (* | Image *)
  [@@deriving sexp_of]
end

let fetch_spec inject =
  let open Effect.Let_syntax in
  let%bind response = Effect.of_deferred_fun (fun p -> Async_js.Http.get p) "/spec" in
  inject (Action.Spec (Or_error.ok_exn response))
;;

let handle_dd_change inject _ev _s = fetch_spec inject

let form_of_v (inject : (Action.t -> unit Effect.t) Value.t) : V.t Form.t Computation.t =
  Form.Typed.Variant.make
    (module struct
      (* reimport the module that typed_fields just derived *)
      module Typed_variant = V.Typed_variant

      (* let label_for_variant = `Inferred *)
      (* provide a form computation for constructor in the variant *)
      let form_for_variant : type a. a Typed_variant.t -> a Form.t Computation.t
        = function
        | Opt125m_output_range -> Bonsai.const (Form.return ())
        | B ->
          Form.Elements.Dropdown.list
            [%here]
            ~extra_attrs:
              (Value.map inject ~f:(fun inject ->
                 [ Vdom.Attr.on_change (handle_dd_change inject) ]))
            (module String)
            (Value.return [ "hello"; "world"; "arul" ])
            ~init:`First_item
        | C -> Form.Elements.Textbox.string [%here]
      ;;
    end)
;;

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
        | Spec s -> { model with spec = Some s })
  in
  let%sub form_v = form_of_v inject in
  let%arr form_v = form_v
  and state = state in
  let value = Form.value form_v in
  Vdom.Node.div
    [ Form.view_as_vdom form_v
    ; Vdom.Node.sexp_for_debugging ([%sexp_of: V.t Or_error.t] value)
    ; Vdom.Node.text (Sexp.to_string (M.sexp_of_t state))
    ]
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" view_of_form
;;
