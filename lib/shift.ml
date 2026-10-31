(* Group-2 shifts and rotates. Count is masked to five bits; zero effective
    count preserves all flags. Undefined OF/AF values are preserved. *)

open Base
module W = Stdlib.Int32

(* Execute a group-2 shift or rotate and update its affected flags. *)
let execute ~width ~kind ~count ~flags value =
  let bits = Width.bits width in
  let masked_count = count land 31 in
  let count =
    if kind < 2 then Int.rem masked_count bits
    else if kind < 4 then Int.rem masked_count (bits + 1)
    else masked_count
  in
  let value = ref (Width.truncate width value) in
  let carry = ref (not (W.equal (W.logand flags 1l) 0l)) in
  let top value =
    not (W.equal (W.logand value (W.shift_left 1l (bits - 1))) 0l)
  in
  let original_sign = top !value in
  if kind >= 4 && count > 0 then (
    let original = !value in
    let left = kind = 4 || kind = 6 in
    carry :=
      if count > bits then kind = 7 && original_sign
      else if left then
        not
          (W.equal
             (W.logand (W.shift_right_logical original (bits - count)) 1l)
             0l)
      else
        not
          (W.equal
             (W.logand (W.shift_right_logical original (count - 1)) 1l)
             0l);
    value :=
      Width.truncate width
        (if kind = 7 then W.shift_right (Width.sign_extend width original) count
         else if count >= bits then 0l
         else if left then W.shift_left original count
         else W.shift_right_logical original count))
  else
    for _ = 1 to count do
      let old_carry = !carry in
      let left = kind = 0 || kind = 2 || kind = 4 || kind = 6 in
      carry :=
        if left then top !value else not (W.equal (W.logand !value 1l) 0l);
      let incoming =
        if kind < 2 then !carry
        else if kind < 4 then old_carry
        else kind = 7 && top !value
      in
      let shifted =
        if left then W.shift_left !value 1 else W.shift_right_logical !value 1
      in
      let bit =
        if incoming then W.shift_left 1l (if left then 0 else bits - 1) else 0l
      in
      value := Width.truncate width (W.logor shifted bit)
    done;
  let set_bit flags bit enabled =
    W.logor (W.logand flags (W.lognot bit)) (if enabled then bit else 0l)
  in
  if kind < 2 && masked_count <> 0 && count = 0 then
    carry :=
      if kind = 0 then not (W.equal (W.logand !value 1l) 0l) else top !value;
  let result_flags =
    if count = 0 && (kind >= 2 || masked_count = 0) then flags
    else
      let flags = set_bit flags 1l !carry in
      let flags =
        if kind < 4 then flags
        else
          let status = Alu.status_flags ~width !value in
          W.logor (W.logand flags (W.lognot 0xc4l)) (W.logand status 0xc4l)
      in
      if masked_count <> 1 then flags
      else
        let overflow =
          if kind = 0 || kind = 2 || kind = 4 || kind = 6 then
            not (Bool.equal (top !value) !carry)
          else if kind = 5 then original_sign
          else if kind = 7 then false
          else not (Bool.equal (top !value) (top (W.shift_left !value 1)))
        in
        set_bit flags 0x800l overflow
  in
  { Alu.value = !value; flags = result_flags }
