
(* MP4 : Some stuff? :p *)

open Hashtbl
open Printf

type action = Recurse | Display of (Bitstring.bitstring -> unit)

let actions = Hashtbl.create 32

let filetype bits =
	()

let media_data bits =
	()

let sample_description bits =
	bitmatch bits with
	| { _ : 32; (* version & flags *)
		0x1l : 32 : bigendian; (* num_entries *)
		_ : 32; (* size *)
		format_id : 32 : string;
		_ : 48; (* reserved *)
		0x1 : 16 : bigendian;
		_ : 64; (* revision level, vendor, reserved *)
		channels_per_frame : 16 : bigendian;
		bits_per_channel : 16 : bigendian;
		_ : 32; (* compression id, packet size *)
		sample_rate : 16 : bigendian;
		_ : 16;
		cookie : -1 : bitstring }
	->
		printf "format id = %s, channels = %d, bitrate = %d, sample rate = %d\n"
			format_id channels_per_frame bits_per_channel sample_rate;
		let cookie = Alac.init cookie in
		Alac.print_specific_config cookie
	| { _ } -> printf "invalid sample description"

let () =
	(* register some actions for some boxes *)
	add actions "ftyp" (Display filetype);
	add actions "moov" Recurse;
	add actions "mdat" (Display media_data);
	add actions "trak" Recurse;
	add actions "udta" Recurse;
	add actions "mdia" Recurse;
	add actions "minf" Recurse;
	add actions "stbl" Recurse;
	add actions "stsd" (Display sample_description)

let rec box indent bits =
	bitmatch bits with
	| { 0x0000_l : 32 : bigendian;
		kind : 32 : string;
		data : -1 : bitstring }
	-> printf "%*slast box: %s\n" indent "" kind; action indent kind data
	| { 0x0001_l : 32 : bigendian;
		kind : 32 : string;
		size : 64 : bigendian;
		data : Int64.to_int size * 8 - 96: bitstring;
		next : -1 : bitstring }
	-> printf "%*slarge box: %s, %Ld bytes\n" indent "" kind size; action indent kind data; box indent next
	| { size : 32 : bigendian;
		kind : 32 : string;
		data : Int32.to_int size * 8 - 64 : bitstring;
		next : -1 : bitstring }
	-> printf "%*ssmall box: %s, %ld bytes\n" indent "" kind size; action indent kind data; box indent next
	| { _ } -> ()
and action indent kind bits =
	try
		match Hashtbl.find actions kind with
		| Recurse -> box (indent + 2) bits
		| Display f -> f bits
	with Not_found -> ()

let run filename =
	box 0 (Bitstring.bitstring_of_file filename)
