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
  [@@deriving sexp, equal, fields]

  let default = { error = None; spec = None; images = []; show_images = false }
end

module V = struct
  type t =
    | Quantization of Quant_view.t
    | Opt125m_output_range
    | Opt_all_output_range
    | Opt125m_heatmap_sensitive_layers of string list
    | Opt_channel_max
    | Opt125m_boxplot
    | Opt350m_boxplot
    | Opt1b_boxplot
    | Opt2b_boxplot
    | Opt125m_fp8_inputs_hist
    | Opt125m_fp8_weights_hist
    | Opt125m_fp8_outputs_hist
    | Opt125m_fp8_inputs_calib
    | Opt125m_fp8_layer_variables_calib
    | Opt125m_vsq_layer_variables_calib
    | Opt125m_vsq_inputs_calib
    | Opt6dot7b_fp8_inputs_hist
    | Opt6dot7b_fp8_weights_hist
    | Opt6dot7b_fp8_outputs_hist
    | Maskformer_fp8_inputs_hist
    | Maskformer_fp8_outputs_hist
    | Maskformer_fp8_weights_hist
    | Vitlarge32_fp8_inputs_hist
    | Vitlarge32_fp8_weights_hist
    | Vitlarge32_fp8_outputs_hist
    | Vitlarge16_fp8_inputs_hist
    | Vitlarge16_fp8_weights_hist
    | Vitlarge16_fp8_outputs_hist
    | Hrnetv2_fp8_inputs_hist
    | Hrnetv2_fp8_weights_hist
    | Hrnetv2_fp8_outputs_hist
    | Pointcloudtransformer_fp8_inputs_hist
    | Pointcloudtransformer_fp8_weights_hist
    | Pointcloudtransformer_fp8_outputs_hist
    | Codegen6dot7b_fp8_inputs_hist
    | Codegen6dot7b_fp8_weights_hist
    | Codegen6dot7b_fp8_outputs_hist
    | Codegen2dot7b_fp8_inputs_hist
    | Codegen2dot7b_fp8_weights_hist
    | Codegen2dot7b_fp8_outputs_hist
  [@@deriving typed_variants, sexp, equal]
end

module Form = Bonsai_web_ui_form

module Action = struct
  type t =
    | Spec of string option
    | Images of string list
    | Error of Error.t option
  [@@deriving sexp_of]
end

let vega_div = "#viz"
let vega_obj_key = "VEGA_DEBUG"

let handle_spec_change s =
  let json_spec =
    Js.Unsafe.fun_call
      (Js.Unsafe.js_expr "JSON.parse")
      [| Js.Unsafe.inject (Js.string s) |]
  in
  (* let vega = Jv.find Jv.global "vega" |> Option.value_exn in *)
  (* let level = Jv.find vega "Debug" |> Option.value_exn in *)
  let vega_embed_opts = Jv.obj [| "renderer", Jv.of_string "svg" |] in
  let vpromise =
    Jv.call Jv.global "vegaEmbed" [| Jv.of_string vega_div; json_spec; vega_embed_opts |]
  in
  let attach_to_global view = Jv.set Jv.global vega_obj_key view in
  (* Fut.of_promise ~ok:attach_to_global vpromise |> await Result.get_ok; *)
  let fut_or_err =
    Fut.of_promise'
      ~ok:(fun a ->
        Brr.Console.(log [ str "vega_loaded"; a ]);
        attach_to_global a;
        a)
      ~error:(fun e -> Brr.Console.(log [ e ]))
      vpromise
  in
  (* Fut.await fut_or_err (fun a -> Brr.Console.(log [ str "await complete"; a ])) *)
  Fut.await fut_or_err Fn.ignore
;;

let fetch_spec ?transform inject s =
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
    let spec = Option.fold transform ~init:spec ~f:(fun spec tf -> tf spec) in
    Stdio.Out_channel.output_string Stdio.stdout spec;
    handle_spec_change spec;
    inject (Action.Spec (Some spec)))
;;

let build_quant_view =
  (*   let open Bonsai.Let_syntax in *)
  Quant_view.form_of_v
;;

(*   (\* let btn = Form.Submit.create ~handle_enter:false ~f:(Quant_view.handle_update) () in *\) *)
(*   (\* let%sub () = *\) *)
(*   (\*   Form.Dynamic.on_change *\) *)
(*   (\*     (module Quant_view) *\) *)
(*   (\*     ~f:(Value.return Quant_view.handle_update) *\) *)
(*   (\*     qv *\) *)
(*   (\* in *\) *)
(*   let%arr qv = qv in *)
(*   qv *)

(* let%arr list_form = list_form in *)
(* let vdtom = (Form.view_as_vdom btn) in *)
(* Form.group'  list_form *)

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
            (Value.return
               [ "model.decoder.layers.1.fc1"
               ; "model.decoder.layers.8.self_attn.v_proj"
               ; "model.decoder.layers.9.self_attn.v_proj"
               ; "model.decoder.layers.7.self_attn.v_proj"
               ; "model.decoder.layers.11.self_attn.v_proj"
               ; "model.decoder.layers.4.self_attn.v_proj"
               ; "model.decoder.layers.10.self_attn.v_proj"
               ; "model.decoder.layers.6.self_attn.v_proj"
               ; "model.decoder.layers.2.fc1"
               ; "model.decoder.layers.1.fc2"
               ])
        | Opt_channel_max -> Bonsai.const (Form.return ())
        | Opt125m_boxplot -> Bonsai.const (Form.return ())
        | Opt350m_boxplot -> Bonsai.const (Form.return ())
        | Opt1b_boxplot -> Bonsai.const (Form.return ())
        | Opt2b_boxplot -> Bonsai.const (Form.return ())
        | Opt125m_fp8_inputs_hist -> Bonsai.const (Form.return ())
        | Opt125m_fp8_weights_hist -> Bonsai.const (Form.return ())
        | Opt125m_fp8_outputs_hist -> Bonsai.const (Form.return ())
        | Opt6dot7b_fp8_inputs_hist -> Bonsai.const (Form.return ())
        | Opt6dot7b_fp8_weights_hist -> Bonsai.const (Form.return ())
        | Opt6dot7b_fp8_outputs_hist -> Bonsai.const (Form.return ())
        | Maskformer_fp8_weights_hist -> Bonsai.const (Form.return ())
        | Maskformer_fp8_inputs_hist -> Bonsai.const (Form.return ())
        | Maskformer_fp8_outputs_hist -> Bonsai.const (Form.return ())
        | Vitlarge32_fp8_inputs_hist -> Bonsai.const (Form.return ())
        | Vitlarge32_fp8_outputs_hist -> Bonsai.const (Form.return ())
        | Vitlarge32_fp8_weights_hist -> Bonsai.const (Form.return ())
        | Vitlarge16_fp8_inputs_hist -> Bonsai.const (Form.return ())
        | Vitlarge16_fp8_outputs_hist -> Bonsai.const (Form.return ())
        | Vitlarge16_fp8_weights_hist -> Bonsai.const (Form.return ())
        (* | Opt125m_fp8_layer_variables_hist -> Bonsai.const (Form.return ()) *)
        | Opt125m_fp8_layer_variables_calib -> Bonsai.const (Form.return ())
        | Opt125m_fp8_inputs_calib -> Bonsai.const (Form.return ())
        | Opt125m_vsq_layer_variables_calib -> Bonsai.const (Form.return ())
        | Opt125m_vsq_inputs_calib -> Bonsai.const (Form.return ())
        | Hrnetv2_fp8_inputs_hist -> Bonsai.const (Form.return ())
        | Hrnetv2_fp8_weights_hist -> Bonsai.const (Form.return ())
        | Hrnetv2_fp8_outputs_hist -> Bonsai.const (Form.return ())
        | Pointcloudtransformer_fp8_inputs_hist -> Bonsai.const (Form.return ())
        | Pointcloudtransformer_fp8_weights_hist -> Bonsai.const (Form.return ())
        | Pointcloudtransformer_fp8_outputs_hist -> Bonsai.const (Form.return ())
        | Codegen6dot7b_fp8_inputs_hist -> Bonsai.const (Form.return ())
        | Codegen6dot7b_fp8_weights_hist -> Bonsai.const (Form.return ())
        | Codegen6dot7b_fp8_outputs_hist -> Bonsai.const (Form.return ())
        | Codegen2dot7b_fp8_inputs_hist -> Bonsai.const (Form.return ())
        | Codegen2dot7b_fp8_weights_hist -> Bonsai.const (Form.return ())
        | Codegen2dot7b_fp8_outputs_hist -> Bonsai.const (Form.return ())
        | Quantization -> build_quant_view
      ;;
    end)
;;

let fetch_spec_for_v inject v =
  fetch_spec inject (Base.String.lowercase (Sexp.to_string (V.sexp_of_t v)))
;;

let transform_boxplot_spec model spec =
  let spec =
    Stringext.replace_all
      spec
      ~pattern:"data/opt_boxplot_stats.csv"
      ~with_:("data/" ^ model ^ "/opt_boxplot_stats.csv")
  in
  let spec =
    Stringext.replace_all
      spec
      ~pattern:"data/opt_boxplot_outliers.csv"
      ~with_:("data/" ^ model ^ "/opt_boxplot_outliers.csv")
  in
  spec
;;

let get_name_and_model v =
  let name = Base.String.lowercase (Sexp.to_string (V.sexp_of_t v)) in
  let parts = String.split name ~on:'_' in
  let model = List.hd_exn parts in
  let model = Stringext.replace_all model ~pattern:"dot" ~with_:"." in
  let filename = String.concat ~sep:"_" (List.tl_exn parts) in
  model, filename, List.tl_exn parts
;;

let transform_hist_spec v spec =
  let model, filename, parts = get_name_and_model v in
  let prefix filename = "data/" ^ model ^ "/quant/" ^ filename in
  let spec =
    Stringext.replace_all
      spec
      ~pattern:"data/replace_me_hist.csv"
      ~with_:(prefix filename ^ ".csv")
  in
  let parts = List.drop_last_exn parts in
  let calib_file = String.concat ~sep:"_" parts in
  let spec =
    Stringext.replace_all
      spec
      ~pattern:"data/replace_me_calib.csv"
      ~with_:(prefix calib_file ^ "_calib" ^ ".csv")
  in
  spec
;;

let transform_quant_spec v spec =
  let model, filename, _ = get_name_and_model v in
  let prefix = "data/" ^ model ^ "/quant/" ^ filename in
  let spec =
    Stringext.replace_all
      spec
      ~pattern:"data/replace_me_calib.csv"
      ~with_:(prefix ^ ".csv")
  in
  spec
;;

let handle_v_change inject v _ =
  match v with
  | Ok v ->
    (match v with
     | V.Opt125m_output_range -> fetch_spec_for_v inject V.Opt125m_output_range
     | V.Opt_all_output_range -> fetch_spec_for_v inject V.Opt_all_output_range
     | V.Opt125m_heatmap_sensitive_layers selected ->
       handle_spec_change "{}";
       inject (Images selected)
     | V.Opt_channel_max -> fetch_spec_for_v inject V.Opt_channel_max
     | V.Opt125m_boxplot ->
       fetch_spec
         ~transform:(transform_boxplot_spec "opt125m")
         inject
         "opt_boxplot" (* Download the generic recipe *)
     | V.Opt350m_boxplot ->
       fetch_spec ~transform:(transform_boxplot_spec "opt350m") inject "opt_boxplot"
     | V.Opt1b_boxplot ->
       fetch_spec ~transform:(transform_boxplot_spec "opt1.3b") inject "opt_boxplot"
     | V.Opt2b_boxplot ->
       fetch_spec ~transform:(transform_boxplot_spec "opt2.7b") inject "opt_boxplot"
     | ( V.Opt125m_fp8_inputs_hist
       | V.Opt125m_fp8_outputs_hist
       | V.Opt125m_fp8_weights_hist
       | V.Opt6dot7b_fp8_inputs_hist
       | V.Opt6dot7b_fp8_weights_hist
       | V.Opt6dot7b_fp8_outputs_hist
       | V.Maskformer_fp8_inputs_hist
       | V.Maskformer_fp8_outputs_hist
       | V.Maskformer_fp8_weights_hist
       | V.Vitlarge32_fp8_inputs_hist
       | V.Vitlarge32_fp8_outputs_hist
       | V.Vitlarge32_fp8_weights_hist
       | V.Vitlarge16_fp8_inputs_hist
       | V.Vitlarge16_fp8_outputs_hist
       | V.Hrnetv2_fp8_inputs_hist
       | V.Hrnetv2_fp8_outputs_hist
       | V.Hrnetv2_fp8_weights_hist
       | V.Vitlarge16_fp8_weights_hist
       | V.Pointcloudtransformer_fp8_inputs_hist
       | V.Pointcloudtransformer_fp8_weights_hist
       | V.Pointcloudtransformer_fp8_outputs_hist
       | V.Codegen6dot7b_fp8_inputs_hist
       | V.Codegen6dot7b_fp8_weights_hist
       | V.Codegen6dot7b_fp8_outputs_hist
       | V.Codegen2dot7b_fp8_inputs_hist
       | V.Codegen2dot7b_fp8_outputs_hist
       | V.Codegen2dot7b_fp8_weights_hist ) as v ->
       fetch_spec ~transform:(transform_hist_spec v) inject "histogram_comp"
     | V.Opt125m_fp8_inputs_calib ->
       fetch_spec
         ~transform:(transform_quant_spec V.Opt125m_fp8_inputs_calib)
         inject
         "quant_error"
     | V.Opt125m_fp8_layer_variables_calib ->
       fetch_spec
         ~transform:(transform_quant_spec V.Opt125m_fp8_layer_variables_calib)
         inject
         "quant_error"
     | V.Opt125m_vsq_inputs_calib ->
       fetch_spec
         ~transform:(transform_quant_spec V.Opt125m_vsq_inputs_calib)
         inject
         "quant_error"
     | V.Opt125m_vsq_layer_variables_calib ->
       Stdio.print_string "vsq save";
       Stdio.Out_channel.flush Stdio.stdout;
       fetch_spec
         ~transform:(transform_quant_spec V.Opt125m_vsq_layer_variables_calib)
         inject
         "quant_error"
     | V.Quantization _xs -> fetch_spec inject "quant_diff_full")
  | _ -> Effect.Ignore
;;

let build_image_path layer_name = "data/opt125m/heatmap/images/" ^ layer_name ^ ".png"

(* let submit_for_v = function *)
(*   | V.Quantization xs -> Form.Submit.create ~f:()
*)

let add_submit v =
  let ok_val = Core.Or_error.ok v in
  match ok_val with
  | Some (V.Quantization xs) ->
    let on_click_attr = Vdom.Attr.on_click (fun _ -> Quant_view.handle_update xs) in
    let title_attr = Vdom.Attr.title "update" in
    let attr = Vdom.Attr.(on_click_attr @ title_attr) in
    Vdom.Node.button ~key:"submit" ~attr [ Vdom.Node.Text "Update Viz" ]
  | _ -> Vdom.Node.none
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
        | Spec s -> { model with spec = s; show_images = false }
        | Images xs -> { model with images = xs; show_images = true }
        | Error e -> { model with error = e; show_images = false })
  in
  let%sub form_v = form_of_v inject in
  (* let%sub () = *)
  (*   Form.Dynamic.on_change *)
  (*     (module V) *)
  (*     form_v *)
  (*     ~f:(Value.map inject ~f:(fun inject -> handle_v_change inject)) *)
  (* in *)
  let%arr form_v = form_v
  and state = state
  and inject = inject in
  let value = Form.value form_v in
  let viz_visible = M.spec state |> Option.is_some in
  let viz_btn_text = if viz_visible then "Switch Viz" else "Show Viz" in
  let viz_btn =
    Vdom.Node.button
      ~attr:(Vdom.Attr.on_click (handle_v_change inject value))
      [ Vdom.Node.Text viz_btn_text ]
  in
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
     ; Vdom.Node.text "internal state for debugging:"
     ; Vdom.Node.sexp_for_debugging ([%sexp_of: V.t Or_error.t] value)
     ; add_submit value (* ; Vdom.Node.text (Sexp.to_string (M.sexp_of_t state)) *)
     ; viz_btn
     ]
    @ images)
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" view_of_form
;;
