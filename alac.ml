
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
	| { _ } -> failf "Couldn't find valid ALAC cookie"
