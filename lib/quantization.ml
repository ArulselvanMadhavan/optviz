include Quant_intf

module FP32_to_VSQ(V : VSQ) : Quant with type t := V.t = struct

  let n_max = Base.Int.(2 ** (V.n_bits - 1) - 1)

  let m_max = Base.Int.(2 ** (V.m_bits) - 1)
                
  let quantize ~fp32 =
    let xxs = Base.List.chunks_of fp32 ~length:V.tile_size in
    let chunk_scales = Base.Array.create ~len:(List.length xxs) 0. in
    let xq = Base.List.mapi xxs ~f:(fun i chunk ->
        let tile_max = Base.List.max_elt chunk ~compare:(Base.Float.compare) |> Option.get in
        let tile_scale = (tile_max /. Float.of_int n_max) in
        chunk_scales.(i) <- tile_scale;
        Base.List.map chunk ~f:(fun x -> (x /. tile_scale) |> Base.Float.round_nearest |> Int.of_float)
      ) in
    let quantize_scales scales =
      let scale_max = Base.List.max_elt scales ~compare:(Base.Float.compare) |> Option.get in
      let gamma = scale_max /. Float.of_int m_max in
      gamma, Base.List.map scales ~f:(fun scale ->
          scale /. gamma |> Base.Float.round_nearest |> Int.of_float
      )
    in
    let gamma, q_scales = quantize_scales (Base.Array.to_list chunk_scales) in
    let combined = Base.List.zip_exn q_scales xq in
    let results = Base.List.map combined ~f:(fun (q_scale, chunk) ->
        Base.List.map chunk ~f:(fun x -> x, Float.of_int (x * q_scale) *. gamma)
      ) in
    Base.List.concat results
end

module FP32_to_INT_Q (I : INT_Q) : Quant with type t := I.t = struct
  let max_bound = Base.Int.(2 ** (I.n_bits - 1)) - 1
  let min_bound = -max_bound

  let max_val xs =
    let abs_vals = Base.List.map xs ~f:Base.Float.abs in
    Base.List.max_elt abs_vals ~compare:Base.Float.compare |> Option.get
  ;;

  let quantize ~fp32 =
    let scale = Float.of_int max_bound /. max_val fp32 in
    let q_helper fp32 =
      let r = fp32 *. scale in
      let r = Base.Float.round_nearest r |> Int.of_float in
      let r =
        if r < min_bound then min_bound else if r > max_bound then max_bound else r
      in
      r
    in
    List.map q_helper fp32
  ;;
end

module FP32_to_FP_Q (F : FP_Q) : Quant with type t := F.t = struct
  let n_bits = F.n_bits
  let mantissa = F.mantissa
  let exponent = n_bits - 1 - mantissa
  let exp_minus_1 = exponent - 1
  let bias_offset = Base.Int.((2 ** exp_minus_1) - 1)
  let mant_rem = 23 - mantissa

  let target_exp tgt =
    let open Unsigned.UInt32 in
    let open Infix in
    let te = ((tgt lsl 1) lsr 1) lsr 23 in
    to_int32 (te - of_int bias_offset)
  ;;

  let min_exp =
    let me = -((1 lsl exp_minus_1) - 2) in
    Base.Int32.of_int_exn me
  ;;

  let round_bitwise target =
    (* Nearest rounding *)
    let m_rem = mant_rem - 1 in
    let open Unsigned.UInt32 in
    let open Infix in
    let one = of_int 1 in
    let mask = (one lsl mant_rem) - one in
    let rand_prob = of_int 1 lsl m_rem in
    let add_r = add target rand_prob in
    logand add_r (lognot mask)
  ;;

  let clip_exponent old_num q_num =
    let open Unsigned.UInt32 in
    let open Infix in
    let clip_up max_exp =
      let max_man = (((of_int (-1) lsl 9) lsr 9) lsr mant_rem) lsl mant_rem in
      let max_num = (max_exp lsl 23) lor max_man in
      let old_sign = (old_num lsr 31) lsl 31 in
      old_sign lor max_num
    in
    let clip_quant () =
      let q_exp_store = ((q_num lsl 1) lsr 1) lsr 23 |> to_int in
      let max_exp_store = Int.(add (shift_left 1 exp_minus_1) 127) in
      if q_exp_store > max_exp_store then clip_up (of_int max_exp_store) else q_num
    in
    if equal q_num zero then q_num else clip_quant ()
  ;;

  let quantize_normal target =
    let quantized_bits = round_bitwise target in
    clip_exponent target quantized_bits |> Unsigned.UInt32.to_int32
  ;;

  let quantize ~fp32 =
    let open Base.Int32 in
    let q_helper fp32 =
      let target_int32 = bits_of_float fp32 in
      let is_signed = is_negative target_int32 in
      let target = Unsigned.UInt32.of_int32 target_int32 in
      let target_exp = target_exp target in
      let is_subnormal = target_exp < min_exp in
      let restore_sign x = if is_signed then Float.neg x else x
      in
      if is_subnormal then fp32 else quantize_normal target |> float_of_bits |> restore_sign
    in
    List.map q_helper fp32
  ;;
end
