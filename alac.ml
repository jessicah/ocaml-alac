
(* ALAC : Decoder *)

let failf args = Printf.kprintf failwith args

type element
= SCE (* single channel element *)
| CPE (* channel pair element *)
| CCE (* coupling channel element *)
| LFE (* LFE channel element *)
| DSE
| PCE
| FIL
| END

let of_element = function
	| SCE -> 0
	| CPE -> 1
	| CCE -> 2
	| LFE -> 3
	| DSE -> 4
	| PCE -> 5
	| FIL -> 6
	| END -> 7
let to_element = function
	| 0 -> SCE
	| 1 -> CPE
	| 2 -> CCE
	| 3 -> LFE
	| 4 -> DSE
	| 5 -> PCE
	| 6 -> FIL
	| 7 -> END
	| n -> failf "invalid element type: %d" n

(* link in the other modules until have a full decoder and don't need this hack :p *)

(*open BitBuffer
open ArrayTypes
open DynamicPredictor
open Matrix
open AdaptiveGolomb*)

(* ALACDecoder.h
	int32 Init (void* inMagicCookier, uint32_t inMagicCookieSize)
	int32 Decode (struct BitBuffer *bits, uint8_t *sampleBuffer, uint32_t numSamples, uint32_t numChannels, uint32_t *outNumSamples)
	
	ALACSpecificConfig mConfig
	
	int32_t FillElement (struct BitBuffer *bits)
	int32_t DataStreamElement (struct BitBuffer *bits)
	
	uint16_t mActiveElements
	
	int32_t *mMixBufferU
	int32_t *mMixBufferV
	int32_t *mPredictor
	uint16_t *mShiftBuffer /* this is a sub-array of mPredictor */
*)

type specific_config = {
	frame_length : int32; (* uint32 *)
	(*compatible_version : int; (* uint8 *) -- has no real use for us *)
	bit_depth : int; (* uint8 *)
	pb : int; (* uint8 *)
	mb : int; (* uint8 *)
	kb : int; (* uint8 *)
	num_channels : int; (* uint8 *)
	max_run : int; (* uint16 *)
	max_frame_bytes : int32; (* uint32 *)
	avg_bit_rate : int32; (* uint32 *)
	sample_rate : int32; (* uint32 *)
}

let print_specific_config cfg =
	Printf.printf (
		"frame length:    %ld\n" ^^
		"bit depth:       %d\n" ^^
		"pb:              %d\n" ^^
		"mb:              %d\n" ^^
		"kb:              %d\n" ^^
		"channels:        %d\n" ^^
		"max run:         %d\n" ^^
		"max frame bytes: %ld\n" ^^
		"avg bitrate:     %ld\n" ^^
		"sample rate:     %ld\n")
	cfg.frame_length cfg.bit_depth cfg.pb cfg.mb cfg.kb cfg.num_channels
	cfg.max_run cfg.max_frame_bytes cfg.avg_bit_rate cfg.sample_rate

let rec init bits =
	bitmatch bits with
	| { _ : 32 : bitstring; "frma" : 32 : string; _ : 32 : bitstring; rest : -1 : bitstring } ->
		(* skip format ('frma') atom if present *)
		init rest
	| { _ : 32 : bitstring; "alac" : 32 : string; _ : 32 : bitstring; rest : -1 : bitstring } ->
		(* skip 'alac' atom header if present *)
		init rest
	| {
		frame_length : 32 : bigendian;
		compatible_version : 8;
		bit_depth : 8;
		pb : 8;
		mb : 8;
		kb : 8;
		num_channels : 8;
		max_run : 16 : bigendian;
		max_frame_bytes : 32 : bigendian;
		avg_bit_rate : 32 : bigendian;
		sample_rate : 32 : bigendian }
		(* ensure version matches *)
		when compatible_version = 0 ->
		(* return the specific_config... the buffers don't matter too much right now *)
		({
			frame_length = frame_length;
			bit_depth = bit_depth;
			pb = pb;
			mb = mb;
			kb = kb;
			num_channels = num_channels;
			max_run = max_run;
			max_frame_bytes = max_frame_bytes;
			avg_bit_rate = avg_bit_rate;
			sample_rate = sample_rate;
		})
	| { _ } -> failwith "alac: missing/invalid cookie"

let fill_element bits =
	let count = match BitBuffer.read_small bits 4 with
	| 15 -> 15 + BitBuffer.read_small bits 8 - 1
	| n -> n
	in
	BitBuffer.advance bits (count * 8)

let data_stream_element bits =
	let element_instance_tag = BitBuffer.read_small bits 4 in
	let data_byte_align_flag = BitBuffer.read_one bits in
	let count = match BitBuffer.read_small bits 8 with
	| 255 -> 255 + BitBuffer.read_small bits 8
	| n -> n
	in
	if data_byte_align_flag <> 0 then BitBuffer.byte_align bits false;
	BitBuffer.advance bits (count * 8)

let zero16 (buffer : ArrayTypes.int16a) num_items stride =
	failwith "alac: shouldn't need; only dealing with stereo files"

(* globals *)

let config = ref {
	frame_length = 0l;
	bit_depth = 0; pb = 0; mb = 0; kb = 0;
	num_channels = 0; max_run = 0;
	max_frame_bytes = 0l;
	avg_bit_rate = 0l;
	sample_rate = 0l;
}

exception Done

(* returns out_num_samples *)
let decode bits (sample_buffer : ArrayTypes.uint8a) num_samples num_channels =
	(* samples = ( int16_t* ) sample_buffer *)
	let num_samples = ref num_samples in
	try while true do
		match to_element (BitBuffer.read_small bits 3) with
		| CPE ->
			(* stereo channel pair *)
			let element_instance_tag = BitBuffer.read_small bits 4 in
			(* don't care about active elements *)

			(* read the 12 unused header bits *)
			let unused_header = BitBuffer.read bits 12 in
			(* assert = 0 *)

			(* read the 1-bit "partial frame" flag, 2-bit "shift-off" flag & 1-bit "escape" flag *)
			let header_byte = BitBuffer.read bits 4 in

			let partial_frame = header_byte lsr 3 in
			let bytes_shifted = (header_byte lsr 1) land 0x3 in
			(* assert != 3 *)
			let shift = bytes_shifted * 8 in
			let escape_flag = header_byte land 0x1 in

			let chan_bits = 16 - (bytes_shifted * 8) + 1 in

			(* check for partial frame length to override requested num_samples *)
			if partial_frame <> 0 then begin
				num_samples := (BitBuffer.read bits 16) lsl 16;
				num_samples := !num_samples lor BitBuffer.read bits 16;
			end;

			if escape_flag = 0 then begin
				(* compressed frame, read rest of parameters *)
				()
			end else begin
				(* uncompressed frame, copy data into the mix buffers to use common output code *)
				()
			end;

			(* now read the shifted values into the shift buffer *)
			if bytes_shifted <> 0 then begin
				let shift = bytes_shifted * 8 in
				(* assert <= 16 *)

				(*for i = 0 to !num_samples - 1 do
					shift_buffer.{i * 2} <- BitBuffer.read shift_bits shift;
					shift_buffer.{i * 2 + 1} <- BitBuffer.read shift_bits shift;
				done*) ()
			end;

			(* un-mix the data and convert to output format *)
			(* - note that mix_res = 0 means just interleave so we use that path for uncompressed frames *)
			(*
				out16 = &((int16_t * )sampleBuffer)[channelIndex];
				unmix16 mix_buffer_u mix_buffer_v out16 num_channels num_samples mix_bits mix_res
			*)

			(* *out_num_samples = num_samples *)

			failwith "not done yet"
		| DSE ->
			(* data stream element -- parse but ignore *)
			data_stream_element bits
		| FIL ->
			(* fill element -- parse but ignore *)
			fill_element bits
		| END ->
			BitBuffer.byte_align bits false;
			raise Done
		| _ -> ()
	done; !num_samples with Done -> !num_samples

let openfile filename =
	let cookie, mdat = Mp4.openfile filename in
	let cookie = init cookie in
	print_specific_config cookie;
	(* set up global config *)
	config := cookie;
	cookie
