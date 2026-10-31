(* Double-width integer arithmetic. Division faults do not produce a result. *)

open Base
module W = Stdlib.Int32
module U = Stdlib.Int64

exception Divide_error

(* The low and high halves of a product and its overflow status. *)
type product = { low : int32; high : int32; overflow : bool }

(* Interpret an int32 as an unsigned 32-bit integer. *)
let unsigned value = U.logand (U.of_int32 value) 0xffffffffL

(* Extend an operand according to its width and signedness. *)
let operand ~width ~signed value =
  if signed then U.of_int32 (Width.sign_extend width value)
  else unsigned (Width.truncate width value)

(* Multiply two operands and report the double-width result and overflow. *)
let multiply ~width ~signed left right =
  let value =
    U.mul (operand ~width ~signed left) (operand ~width ~signed right)
  in
  let low = Width.truncate width (U.to_int32 value) in
  let high =
    Width.truncate width
      (U.to_int32 (U.shift_right_logical value (Width.bits width)))
  in
  let overflow =
    if signed then not (U.equal value (operand ~width ~signed:true low))
    else not (W.equal high 0l)
  in
  { low; high; overflow }

(* Divide a double-width dividend, checking for division faults. *)
let divide ~width ~signed ~high ~low divisor =
  let bits = Width.bits width in
  let dividend =
    U.logor
      (U.shift_left (operand ~width ~signed:false high) bits)
      (operand ~width ~signed:false low)
  in
  let dividend =
    if signed && bits < 32 then
      U.shift_right (U.shift_left dividend (64 - (2 * bits))) (64 - (2 * bits))
    else dividend
  in
  let divisor = operand ~width ~signed divisor in
  if
    U.equal divisor 0L
    || (signed && U.equal dividend U.min_int && U.equal divisor (-1L))
  then raise Divide_error;
  let quotient, remainder =
    if signed then (U.div dividend divisor, U.rem dividend divisor)
    else (U.unsigned_div dividend divisor, U.unsigned_rem dividend divisor)
  in
  let low_result = Width.truncate width (U.to_int32 quotient) in
  if not (U.equal quotient (operand ~width ~signed low_result)) then
    raise Divide_error;
  (low_result, Width.truncate width (U.to_int32 remainder))
