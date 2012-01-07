
(* ALAC: Adaptive Golomb Decoding *)

open ArrayTypes
open Int32

exception ALAC_parameter_error

type params = {
  mutable mb : int32;
  mutable mb0 : int32;
  mutable pb : int32;
  mutable kb : int32;
  mutable wb : int32;
  mutable qb : int32;
  mutable fw : int32;
  mutable sw : int32;
  mutable maxrun : int32;
}

let make_params ~mb:mb ~pb:pb ~kb:kb ~fw:fw ~sw:sw ~maxrun:maxrun =
  let wb = pred (shift_left one (kb))
  and qb = sub (shift_left one 9) (of_int pb)
  in {
    mb = of_int mb;
    mb0 = of_int mb;
    pb = of_int pb;
    kb = of_int kb;
    wb = wb;
    qb = qb;
    fw = of_int fw;
    sw = of_int sw;
    maxrun = of_int maxrun
  }

let std_params ~fw:fw ~sw:sw =
  let mb = 10
  and pb = 40
  and kb = 14
  and maxrun = 255
  in make_params ~mb:mb ~pb:pb ~kb:kb ~fw:fw ~sw:sw ~maxrun:maxrun

let lead m =
  let rec loop c j =
    if logand c m <> zero || j = 32
    then j
    else loop (shift_right_logical c 1) (j+1)in
  loop (shift_left one 31) 0

let lg3a x =
  31 - lead (add x (of_int 3))

let read32bit (buf : uint8a) n =
  let read32 i s k = logor (shift_left (of_int (buf.{n+i})) s) k in
  read32 0 24 (read32 1 16 (read32 2 8 (read32 3 0 zero)))

let get_next_fromlong inlong suff =
  shift_right_logical inlong (32 - suff)

let getstreambits (buf : uint8a) bitoffset numbits =
  let byteoffset = bitoffset / 8 in

	let load1 = read32bit buf byteoffset in

	let result = if (numbits + (bitoffset land 0x7)) > 32 then begin
			let result = shift_left load1 (bitoffset land 0x7) in
			let load2 = of_int buf.{byteoffset + 4} in
			let load2shift = 8 - (numbits + (bitoffset land 0x7)-32) in
			let load2 = shift_right_logical load2 load2shift in
			let result = shift_right_logical result (32-numbits) in
			logor result load2
		end else begin
			shift_right_logical load1 (32-numbits-(bitoffset land 0x7))
		end
	in
	if numbits <> 32 then
		logand result (lognot (shift_left 0xffff_ffffl numbits))
	else result

(* k is used as a shift amount so has to be small, hence not an int32 *)
let dyn_get (buf : uint8a) (bitPos : int) (m : int32) (k : int) =
  let max_prefix_16 = 9
  and max_datatype_bits_16 = 16
  and streamlong =
    shift_left (read32bit buf (bitPos lsr 3))
      (bitPos land 7) in
	(*Printf.printf ">>>%!";*)
  let pre = lead (lognot streamlong)
  in
	(*Printf.printf "<<<%!";*)
  if pre >= max_prefix_16
  then
    let pre = max_prefix_16 in
    let bitPos = bitPos + (pre + max_datatype_bits_16) in
    let streamlong = shift_left streamlong pre
    in (bitPos, get_next_fromlong streamlong max_datatype_bits_16)
  else
    let bitPos = bitPos + (pre + k + 1) in
    let streamlong = shift_left streamlong (pre + 1) in
    let v = get_next_fromlong streamlong k in
    if v < of_int 2
    then (bitPos-1, mul (of_int pre) m)
    else (bitPos, add (mul (of_int pre) m) (pred v))

let dyn_get_32bit (buf : uint8a) (bitPos : int) (m : int32) (k : int) (maxbits : int) =
  let max_prefix_32 = 9
  and streamlong =
    shift_left (read32bit buf (bitPos lsr 3))
      (bitPos land 7) in
  let result = lead (lognot streamlong)
  in
  if result >= max_prefix_32
  then
    let result = getstreambits buf (bitPos + max_prefix_32) maxbits
    in bitPos + (maxbits + max_prefix_32), result
  else
    let bitPos = bitPos + (result + 1) in
    if k <> 1
    then
      let streamlong = shift_left streamlong (result+1) in
      let v = get_next_fromlong streamlong k in
      let bitPos = bitPos + (k-1) in
      let result = mul (of_int result) m in
      if v >= of_int 2
      then (bitPos+1, add result (pred v))
      else (bitPos, result)
    else (bitPos, of_int result)

let qbshift = 9
let qb = shift_left one qbshift
let mmulshift = 2
let mdenshift = qbshift - mmulshift - 1
let moff = 1 lsl (mdenshift - 2)
let bitoff = 24
let n_max_mean_clamp = 0xffffl
let n_mean_clamp_val = 0xffffl

(* out_num_bits is never used, max_size <= 16 *)
let dyn_decomp params bitstream (pc : int32a) num_samples max_size =
	let c = ref 0 in
	let mb = ref params.mb0 in
	let zmode = ref zero in
	let pb_local = params.pb in
	let kb_local = to_int params.kb in
	let wb_local = params.wb in
	(*let bit_pos = ref 0 in
	let in' = BigarrayUtils.int32_to_uint8 pc in*)
	(* bitstream is a BitBuffer *)
	let in' = BigarrayUtils.from_string (bitstream.BitBuffer.buffer) in
	let bit_pos = ref (bitstream.BitBuffer.current + bitstream.BitBuffer.bit_index) in
	let out = ref 0 in
	while !c < num_samples do
		(*Printf.printf "dyn_decomp: %d of %d samples%!" !c num_samples;*)
		let m = shift_right !mb qbshift in
		let k = lg3a m in
		(*Printf.printf " => lg3a%!";*)

		let k = if k < kb_local then k else kb_local in
		let m = sub (shift_left one k) one in
		
		let n = zero in (* dyn_get_32bit (in, &bitPos, m, k, max_size) *)
		let newpos, n = dyn_get_32bit in' !bit_pos m k max_size in
		(*Printf.printf " => dyn_get_32bit%!";*)
		bit_pos := newpos;

		let ndecode = add n !zmode in
		let multiplier = neg (logand ndecode one) (*-(ndecode land 1)*) in
		let multiplier = logor multiplier one in
		(*let del = ((ndecode+1) lsr 1) * multiplier in*)
		let del = mul (shift_right_logical (add ndecode one) 1) multiplier in

		(* *outPtr++ = del; *)
		pc.{!out} <- del; incr out;

		incr c;

		(*mb := pb_local*(n + !zmode) + !mb - ((pb_local * !mb) asr qbshift);*)
		mb := add (mul pb_local (add n !zmode)) (sub !mb (shift_right (mul pb_local !mb) qbshift));

		(* update mean tracking *)
		if n > n_max_mean_clamp then
			mb := n_mean_clamp_val;

		zmode := zero;

		(*if ((!mb lsl mmulshift) < qb) && (!c < num_samples) then begin*)
		if ((shift_left !mb mmulshift) < qb) && (!c < num_samples) then begin
			zmode := one;
			let k = (lead !mb) - bitoff + ((to_int !mb + moff) asr mdenshift) in (* asr or lsr? *)
			(*let mz = ((1 lsl k)-1) land wb_local in*)
			let mz = logand (sub (shift_left one k) one) wb_local in

			let n = 0 in (* dyn_get (in, &bitPos, mz, k) *)
			let newpos, n = dyn_get in' !bit_pos mz k in
			(*Printf.printf " => dyn_get(%ld)%!" n;*)
			bit_pos := newpos;

			begin try for j = 0 to to_int n - 1 do
				(* *outPtr++ = 0; *)
				pc.{!out} <- zero; incr out;
				incr c;
			done; with _ -> () end;

			if n >= 65535l then zmode := zero;

			mb := zero;
		end;
		(*Printf.printf ".\n%!";*)
	done;

	(*BitBuffer.advance bitstream (bit_pos - start_pos)*)
	BitBuffer.advance bitstream !bit_pos
