
(* ALAC: Adaptive Golomb Decoding *)

open ArrayTypes

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
  let open Int32 in
  let wb = pred (shift_left one (to_int kb))
  and qb = sub (shift_left one 9) pb
  in {
    mb = mb;
    mb0 = mb;
    pb = pb;
    kb = kb;
    wb = wb;
    qb = qb;
    fw = fw;
    sw = sw;
    maxrun = maxrun
  }

let std_params ~fw:fw ~sw:sw =
  let open Int32 in
  let mb = of_int 10
  and pb = of_int 40
  and kb = of_int 14
  and maxrun = of_int 255
  in make_params ~mb:mb ~pb:pb ~kb:kb ~fw:fw ~sw:sw ~maxrun:maxrun

let lead m =
  let open Int32 in
  let rec loop c j =
    if logand c m <> zero
    then j
    else loop (shift_right_logical c 1) (j+1)in
  loop (shift_left one 31) 0

let lg3a x =
  let open Int32 in
  31 - lead (add x (of_int 3))

let read32bit (buf : uint8a) n =
  let open Int32 in
  let read32 i s k = add (shift_left (of_int (buf.{n+i})) s) k in
  read32 0 24 (read32 1 16 (read32 2 8 (read32 3 0 zero)))

let get_next_fromlong inlong suff =
  let open Int32 in
  shift_right_logical inlong (32 - suff)

let getstreambits (buf : uint8a) bitoffset numbits =
  failwith "getstreambits"

(* k is used as a shift amount so has to be small, hence not an int32 *)
let dyn_get (buf : uint8a) (bitPos : int32) (m : int32) (k : int) =
  let open Int32 in
  let max_prefix_16 = 9
  and max_datatype_bits_16 = 16
  and streamlong =
    shift_left (read32bit buf (to_int (shift_right_logical bitPos 3)))
      (to_int (logand bitPos (of_int 7))) in
  let pre = lead (lognot streamlong)
  in
  if pre >= max_prefix_16
  then
    let pre = max_prefix_16 in
    let bitPos = add bitPos (of_int (pre + max_datatype_bits_16)) in
    let streamlong = shift_left streamlong pre
    in (bitPos, get_next_fromlong streamlong max_datatype_bits_16)
  else
    let bitPos = add bitPos (of_int (pre + k + 1)) in
    let streamlong = shift_left streamlong (pre + 1) in
    let v = get_next_fromlong streamlong k in
    if v < of_int 2
    then (pred bitPos, mul (of_int pre) m)
    else (bitPos, add (mul (of_int pre) m) (pred v))

let dyn_get_32bit (buf : uint8a) (bitPos : int32) (m : int32) (k : int) (maxbits : int) =
  let open Int32 in
  let max_prefix_32 = 9
  and streamlong =
    shift_left (read32bit buf (to_int (shift_right_logical bitPos 3)))
      (to_int (logand bitPos (of_int 7))) in
  let result = lead (lognot streamlong)
  in
  if result >= max_prefix_32
  then
    let result = getstreambits buf (add bitPos (of_int max_prefix_32)) maxbits
    in (add bitPos (of_int (maxbits + max_prefix_32)), result)
  else
    let bitPos = add bitPos (of_int (result + 1)) in
    if k <> 1
    then
      let streamlong = shift_left streamlong (result+1) in
      let v = get_next_fromlong streamlong k in
      let bitPos = add bitPos (of_int (k-1)) in
      let result = mul (of_int result) m in
      if v >= of_int 2
      then (succ bitPos, add result (pred v))
      else (bitPos, result)
    else (bitPos, of_int result)
