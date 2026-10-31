(* Exact finite binary80 arithmetic with x87 precision/rounding control.
    Nonfinite operands and exceptional results are explicitly unsupported. *)

(* Finite binary80 arithmetic uses exact rational intermediates. *)
type t = { negative : bool; significand : Z.t; exponent : int }

exception Unsupported of string

let decode bytes =
  let bits = Z.of_bits (String.sub bytes 0 8) in
  let exponent =
    Char.code bytes.[8] lor ((Char.code bytes.[9] land 127) lsl 8)
  in
  if exponent = 32767 then raise (Unsupported "x87 infinity/NaN arithmetic");
  {
    negative = Char.code bytes.[9] land 128 <> 0;
    significand = bits;
    exponent = max 1 exponent - 16383 - 63;
  }

let rational value =
  let n =
    if value.negative then Z.neg value.significand else value.significand
  in
  if value.exponent >= 0 then Q.of_bigint (Z.shift_left n value.exponent)
  else Q.make n (Z.shift_left Z.one (-value.exponent))

let encode ?(negative_zero = false) ~control value =
  let negative = Q.sign value < 0 || (Q.equal value Q.zero && negative_zero) in
  let n = Z.abs (Q.num value) and d = Q.den value in
  let precision =
    match (control lsr 8) land 3 with 0 -> 24 | 2 -> 53 | _ -> 64
  in
  let exponent = ref (Z.numbits n - Z.numbits d) in
  let less =
    if !exponent >= 0 then Z.compare n (Z.shift_left d !exponent) < 0
    else Z.compare (Z.shift_left n (- !exponent)) d < 0
  in
  if less then decr exponent;
  let shift = max (-16445) (!exponent - precision + 1) in
  let numerator, denominator =
    if shift >= 0 then (n, Z.shift_left d shift)
    else (Z.shift_left n (-shift), d)
  in
  let quotient, remainder = Z.ediv_rem numerator denominator in
  let round_up =
    match (control lsr 10) land 3 with
    | 0 ->
        let comparison = Z.compare (Z.shift_left remainder 1) denominator in
        comparison > 0 || (comparison = 0 && Z.testbit quotient 0)
    | 1 -> negative && not (Z.equal remainder Z.zero)
    | 2 -> (not negative) && not (Z.equal remainder Z.zero)
    | _ -> false
  in
  let significand = if round_up then Z.succ quotient else quotient in
  let length = Z.numbits significand in
  let exponent = if length = 0 then 0 else length - 1 + shift + 16383 in
  if exponent >= 32767 then raise (Unsupported "x87 arithmetic overflow");
  let significand, exponent =
    if exponent <= 0 then (Z.shift_left significand (shift + 16445), 0)
    else
      ( (if length > 64 then Z.shift_right significand (length - 64)
         else Z.shift_left significand (64 - length)),
        exponent )
  in
  let bytes = Bytes.make 10 '\000' in
  for i = 0 to 7 do
    Bytes.set bytes i (Char.chr (Z.to_int (Z.extract significand (i * 8) 8)))
  done;
  Bytes.set bytes 8 (Char.chr (exponent land 255));
  Bytes.set bytes 9
    (Char.chr ((exponent lsr 8) lor if negative then 128 else 0));
  Bytes.to_string bytes

let of_int64 value = encode ~control:0x37f (Q.of_bigint (Z.of_int64 value))

let binary ~control ~operation a b =
  let left = decode a and right = decode b in
  let negative_zero =
    if operation = 1 || operation >= 6 then left.negative <> right.negative
    else
      let left_sign, right_sign =
        match operation with
        | 4 -> (left.negative, not right.negative)
        | 5 -> (right.negative, not left.negative)
        | _ -> (left.negative, right.negative)
      in
      if
        Z.equal left.significand Z.zero
        && Z.equal right.significand Z.zero
        && left_sign = right_sign
      then left_sign
      else (control lsr 10) land 3 = 1
  in
  let a = rational left and b = rational right in
  let value =
    match operation with
    | 0 -> Q.add a b
    | 1 -> Q.mul a b
    | 4 -> Q.sub a b
    | 5 -> Q.sub b a
    | 6 ->
        if Q.equal b Q.zero then raise (Unsupported "x87 division by zero");
        Q.div a b
    | 7 ->
        if Q.equal a Q.zero then raise (Unsupported "x87 division by zero");
        Q.div b a
    | _ -> invalid_arg "Extended.binary"
  in
  encode ~negative_zero ~control value

let compare a b = Q.compare (rational (decode a)) (rational (decode b))

let of_ieee bytes =
  let bits = Z.of_bits bytes in
  let fraction, exponent_bits, bias =
    if String.length bytes = 4 then (23, 8, 127) else (52, 11, 1023)
  in
  let exponent = Z.to_int (Z.extract bits fraction exponent_bits) in
  if exponent = (1 lsl exponent_bits) - 1 then
    raise (Unsupported "x87 nonfinite load");
  let significand = Z.extract bits 0 fraction in
  let significand =
    if exponent = 0 then significand
    else Z.logor significand (Z.shift_left Z.one fraction)
  in
  let value =
    {
      negative = Z.testbit bits (fraction + exponent_bits);
      significand;
      exponent = max 1 exponent - bias - fraction;
    }
  in
  encode ~negative_zero:value.negative ~control:0x37f (rational value)

let to_ieee ~control ~size bytes =
  let fraction, exponent_bits, bias, precision =
    if size = 4 then (23, 8, 127, 0) else (52, 11, 1023, 2)
  in
  let rounded =
    decode
      (encode ~negative_zero:(decode bytes).negative
         ~control:(control land lnot 0x300 lor (precision lsl 8))
         (rational (decode bytes)))
  in
  let zero = Z.equal rounded.significand Z.zero in
  let exponent = if zero then 0 else rounded.exponent + 63 + bias in
  if ((not zero) && exponent <= 0) || exponent >= (1 lsl exponent_bits) - 1 then
    raise (Unsupported "x87 IEEE conversion range");
  let bits =
    Z.logor
      (Z.shift_left (Z.of_int exponent) fraction)
      (Z.extract rounded.significand (63 - fraction) fraction)
  in
  let bits =
    if rounded.negative then
      Z.logor bits (Z.shift_left Z.one (fraction + exponent_bits))
    else bits
  in
  String.init size (fun index ->
      Char.chr (Z.to_int (Z.extract bits (index * 8) 8)))

let to_integer ~control ~bits bytes =
  let value = rational (decode bytes) in
  let n = Q.num value and d = Q.den value in
  let negative = Z.sign n < 0 in
  let q, r = Z.ediv_rem (Z.abs n) d in
  let up =
    match (control lsr 10) land 3 with
    | 0 ->
        let c = Z.compare (Z.shift_left r 1) d in
        c > 0 || (c = 0 && Z.testbit q 0)
    | 1 -> negative && not (Z.equal r Z.zero)
    | 2 -> (not negative) && not (Z.equal r Z.zero)
    | _ -> false
  in
  let q = if up then Z.succ q else q in
  let q = if negative then Z.neg q else q in
  let bound = Z.shift_left Z.one (bits - 1) in
  if Z.compare q (Z.neg bound) < 0 || Z.compare q bound >= 0 then
    raise (Unsupported "x87 integer conversion overflow");
  String.init (bits / 8) (fun index ->
      Char.chr (Z.to_int (Z.extract q (8 * index) 8)))
