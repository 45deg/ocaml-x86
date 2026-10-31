type t =
  | Byte
  | Word
  | Dword

let bits = function
  | Byte -> 8
  | Word -> 16
  | Dword -> 32
;;

let bytes t = bits t / 8

let mask = function
  | Byte -> 0xffl
  | Word -> 0xffffl
  | Dword -> -1l
;;

let truncate t value = Int32.logand value (mask t)

let sign_extend t value =
  let shift = 32 - bits t in
  Int32.shift_right (Int32.shift_left value shift) shift
;;
