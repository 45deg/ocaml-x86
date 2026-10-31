(* The supported x86 operand widths. *)
type t = Byte | Word | Dword

(* Return the number of bits in an operand width. *)
let bits = function Byte -> 8 | Word -> 16 | Dword -> 32
(* Return the number of bytes in an operand width. *)
let bytes t = bits t / 8
(* Return a bit mask covering an operand width. *)
let mask = function Byte -> 0xffl | Word -> 0xffffl | Dword -> -1l
(* Discard bits outside the operand width. *)
let truncate t value = Int32.logand value (mask t)

(* Sign-extend a value from the operand width to 32 bits. *)
let sign_extend t value =
  let shift = 32 - bits t in
  Int32.shift_right (Int32.shift_left value shift) shift
