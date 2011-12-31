
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

