(* Exact finite binary80 arithmetic with x87 precision/rounding control.
   NaN/infinity transfers are supported; nonfinite arithmetic and unmasked
   signaling NaN exceptions remain explicitly unsupported. *)

(* Finite binary80 arithmetic uses exact rational intermediates. *)
(* A finite binary80 value represented by sign, significand, and exponent. *)
type t = { negative : bool; significand : Z.t; exponent : int }

exception Unsupported of string

(* Decode a finite binary80 byte string. *)
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

(* Convert a decoded binary80 value to an exact rational. *)
let rational value =
  let n =
    if value.negative then Z.neg value.significand else value.significand
  in
  if value.exponent >= 0 then Q.of_bigint (Z.shift_left n value.exponent)
  else Q.make n (Z.shift_left Z.one (-value.exponent))

(* Round a nonnegative ratio to an integer using the x87 rounding direction. *)
let round_ratio ~control ~negative numerator denominator =
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
  if round_up then Z.succ quotient else quotient

(* Round and encode a rational as binary80 using the x87 control word. *)
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
  let significand = round_ratio ~control ~negative numerator denominator in
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

(* Convert a signed 64-bit integer to binary80. *)
let of_int64 value = encode ~control:0x37f (Q.of_bigint (Z.of_int64 value))

(* Apply an arithmetic operation to binary80 values with x87 rounding. *)
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

(* Compare two finite binary80 values. *)
let compare a b = Q.compare (rational (decode a)) (rational (decode b))

(* One exact FPREM reduction. Quotient bits are meaningful only on completion. *)
type remainder = { value : string; complete : bool; quotient_bits : int }

(* Finite FPREM for ocamlopt-oriented guests and musl fmodl. Process 32--63
   exponent bits at a time; PC/RC do not affect this exact operation. *)
let remainder dividend divisor =
  let left = decode dividend and right = decode divisor in
  if Z.equal right.significand Z.zero then
    raise (Unsupported "x87 FPREM zero divisor");
  let exponent value = value.exponent + Z.numbits value.significand - 1 in
  let difference = exponent left - exponent right in
  let shift =
    if difference < 64 then 0 else difference - (32 + (difference mod 32))
  in
  let modulus = rational { right with exponent = right.exponent + shift } in
  let number = rational left in
  let ratio = Q.div number modulus in
  let quotient = Z.div (Q.num ratio) (Q.den ratio) in
  let result = Q.sub number (Q.mul (Q.of_bigint quotient) modulus) in
  {
    value = encode ~negative_zero:left.negative ~control:0x37f result;
    complete = shift = 0;
    quotient_bits = Z.to_int (Z.extract (Z.abs quotient) 0 3);
  }

(* Serialize a little-endian floating-point encoding without host float operations. *)
let bytes_of_bits ~size bits =
  String.init size (fun index ->
      Char.chr (Z.to_int (Z.extract bits (index * 8) 8)))

(* Identify a signaling NaN in an IEEE single/double encoding. *)
let ieee_signaling_nan bytes =
  let fraction, exponent_bits =
    if String.length bytes = 4 then (23, 8) else (52, 11)
  in
  let bits = Z.of_bits bytes in
  Z.equal
    (Z.extract bits fraction exponent_bits)
    (Z.of_int ((1 lsl exponent_bits) - 1))
  && (not (Z.equal (Z.extract bits 0 fraction) Z.zero))
  && not (Z.testbit bits (fraction - 1))

(* NaN/infinity transfers for ocamlopt-oriented guests and the OCaml runtime
   (Int64.float_of_bits). Preserve signs and payloads, quieting signaling NaNs.
   Fpu.load_ieee handles the invalid-operation flag and exception mask. *)
let of_ieee bytes =
  let bits = Z.of_bits bytes in
  let fraction, exponent_bits, bias =
    if String.length bytes = 4 then (23, 8, 127) else (52, 11, 1023)
  in
  let exponent = Z.to_int (Z.extract bits fraction exponent_bits) in
  let significand = Z.extract bits 0 fraction in
  let negative = Z.testbit bits (fraction + exponent_bits) in
  if exponent = (1 lsl exponent_bits) - 1 then
    let significand =
      if Z.equal significand Z.zero then significand
      else Z.logor significand (Z.shift_left Z.one (fraction - 1))
    in
    let bits =
      Z.logor
        (Z.shift_left (Z.of_int 0x7fff) 64)
        (Z.logor (Z.shift_left Z.one 63)
           (Z.shift_left significand (63 - fraction)))
    in
    let bits =
      if negative then Z.logor bits (Z.shift_left Z.one 79) else bits
    in
    bytes_of_bits ~size:10 bits
  else
    let significand =
      if exponent = 0 then significand
      else Z.logor significand (Z.shift_left Z.one fraction)
    in
    let value =
      { negative; significand; exponent = max 1 exponent - bias - fraction }
    in
    encode ~negative_zero:value.negative ~control:0x37f (rational value)

(* Round and convert binary80 to an IEEE floating-point encoding. *)
let to_ieee ~control ~size bytes =
  let fraction, exponent_bits, bias, precision =
    if size = 4 then (23, 8, 127, 0) else (52, 11, 1023, 2)
  in
  let encoded = Z.of_bits bytes in
  let bits =
    if Z.equal (Z.extract encoded 64 15) (Z.of_int 0x7fff) then (
      if not (Z.testbit encoded 63) then
        raise (Unsupported "x87 unsupported nonfinite encoding");
      let payload = Z.extract encoded 0 63 in
      if (not (Z.equal payload Z.zero)) && not (Z.testbit payload 62) then
        raise (Unsupported "x87 signaling NaN store");
      Z.logor
        (Z.shift_left (Z.of_int ((1 lsl exponent_bits) - 1)) fraction)
        (Z.extract payload (63 - fraction) fraction))
    else
      let value = decode bytes in
      let number = rational value in
      let n = Z.abs (Q.num number) and d = Q.den number in
      if Z.compare (Z.shift_left n (bias - 1)) d < 0 then
        (* ocamlopt-oriented guests also transfer subnormals. Round the original
           value once to the IEEE subnormal quantum, retaining signed zero. *)
        round_ratio ~control ~negative:value.negative
          (Z.shift_left n (bias + fraction - 1))
          d
      else
        let rounded =
          decode
            (encode ~negative_zero:value.negative
               ~control:(control land lnot 0x300 lor (precision lsl 8))
               number)
        in
        let exponent = rounded.exponent + 63 + bias in
        if exponent >= (1 lsl exponent_bits) - 1 then
          raise (Unsupported "x87 IEEE conversion range");
        Z.logor
          (Z.shift_left (Z.of_int exponent) fraction)
          (Z.extract rounded.significand (63 - fraction) fraction)
  in
  let bits =
    if Z.testbit encoded 79 then
      Z.logor bits (Z.shift_left Z.one (fraction + exponent_bits))
    else bits
  in
  bytes_of_bits ~size bits

(* Round a binary80 value to a signed integer of the requested width. *)
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
