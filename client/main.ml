open! Core
open! Bonsai_web

type v =
  | A
  | B of int
  | C of string
[@@deriving typed_variants, sexp_of]

module Form = Bonsai_web_ui_form

let form_of_v : v Form.t Computation.t =
  Form.Typed.Variant.make
    (module struct
      (* reimport the module that typed_fields just derived *)
      module Typed_variant = Typed_variant_of_v

      (* let label_for_variant = `Inferred *)

      (* provide a form computation for constructor in the variant *)
      let form_for_variant : type a. a Typed_variant.t -> a Form.t Computation.t
        = function
        | A -> Bonsai.const (Form.return ())
        | B -> Form.Elements.Textbox.int [%here]
        | C -> Form.Elements.Textbox.string [%here]
      ;;
    end)
;;

let view_of_form : Vdom.Node.t Computation.t =
  let open! Bonsai.Let_syntax in
  let%sub form_v = form_of_v in
  let%arr form_v = form_v in
  let value = Form.value form_v in
  Vdom.Node.div
    [ Form.view_as_vdom form_v
    ; Vdom.Node.sexp_for_debugging ([%sexp_of: v Or_error.t] value)
    ]
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" view_of_form
;;
