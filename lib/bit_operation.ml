open Base
module W = Stdlib.Int32

(* Find the first or last set bit within the operand width. *)
let scan ~reverse ~width value =
  let value = Width.truncate width value in
  let rec loop index =
    if index < 0 || index >= Width.bits width then None
    else if not (W.equal (W.logand value (W.shift_left 1l index)) 0l) then
      Some index
    else loop (index + if reverse then -1 else 1)
  in
  loop (if reverse then Width.bits width - 1 else 0)

(* Reverse the byte order of a 32-bit value. *)
let byte_swap value =
  let result = ref 0l in
  for byte = 0 to 3 do
    result :=
      W.logor !result
        (W.shift_left
           (W.logand (W.shift_right_logical value (byte * 8)) 255l)
           ((3 - byte) * 8))
  done;
  !result

(* Shift bits between two operands and update the affected flags. *)
let double_shift ~width ~right ~count ~flags ~destination ~source =
  let count = count land 31 in
  let bits = Width.bits width in
  let value = ref (Width.truncate width destination) in
  let source = ref (Width.truncate width source) in
  let carry = ref false in
  let top value =
    not (W.equal (W.logand value (W.shift_left 1l (bits - 1))) 0l)
  in
  let original_sign = top !value in
  for _ = 1 to count do
    carry := if right then not (W.equal (W.logand !value 1l) 0l) else top !value;
    let incoming =
      if right then W.shift_left (W.logand !source 1l) (bits - 1)
      else W.shift_right_logical !source (bits - 1)
    in
    value :=
      Width.truncate width
        (W.logor incoming
           (if right then W.shift_right_logical !value 1
            else W.shift_left !value 1));
    source :=
      Width.truncate width
        (if right then W.shift_right_logical !source 1
         else W.shift_left !source 1)
  done;
  let flags =
    if count = 0 then flags
    else
      let status = (Alu.logand ~width !value !value).flags in
      let flags =
        W.logor
          (W.logand flags (W.lognot 0xc5l))
          (W.logor (W.logand status 0xc4l) (if !carry then 1l else 0l))
      in
      if count <> 1 then flags
      else
        W.logor
          (W.logand flags (W.lognot 0x800l))
          (if Bool.equal original_sign (top !value) then 0l else 0x800l)
  in
  { Alu.value = !value; flags }
