(* Legacy x87 state. Register payloads retain all 80 bits across save/restore. *)

open Base

(* x87 control, status, tag, instruction metadata, and register stack state. *)
type t = {
  mutable control : int;
  mutable status : int;
  mutable tag : int;
  mutable instruction : int32;
  mutable code_segment : int;
  mutable opcode : int;
  mutable data : int32;
  mutable data_segment : int;
  registers : string array;
}

(* Reset the x87 state to its initial values. *)
let reset t =
  t.control <- 0x37f;
  t.status <- 0;
  t.tag <- 0xffff;
  t.instruction <- 0l;
  t.code_segment <- 0;
  t.opcode <- 0;
  t.data <- 0l;
  t.data_segment <- 0

(* Create an initialized x87 state. *)
let create () =
  {
    control = 0x37f;
    status = 0;
    tag = 0xffff;
    instruction = 0l;
    code_segment = 0;
    opcode = 0;
    data = 0l;
    data_segment = 0;
    registers = Array.create ~len:8 (String.make 10 '\000');
  }

(* Return the x87 control word. *)
let control t = t.control
(* Set the supported bits of the x87 control word. *)
let set_control t value = t.control <- value land 0x1f3f lor 0x40
(* Return the x87 status word. *)
let status t = t.status
(* Clear the x87 exception status bits. *)
let clear_exceptions t = t.status <- t.status land 0x7f00

(* Convert IEEE operands for ocamlopt-oriented guests. A masked signaling NaN
   becomes a quiet NaN and sets IE; unmasked invalid exceptions are unsupported. *)
let load_ieee t bytes =
  let signaling = Extended.ieee_signaling_nan bytes in
  if signaling && t.control land 1 = 0 then
    raise (Extended.Unsupported "x87 unmasked signaling NaN load");
  let value = Extended.of_ieee bytes in
  if signaling then t.status <- t.status lor 1;
  value

(* Return the saved environment size for the operand width. *)
let environment_size = function Width.Word -> 14 | _ -> 28

(* Serialize the x87 environment for the operand width. *)
let environment t ~operand_size =
  let wide = not (Poly.equal operand_size Width.Word) in
  let bytes = Bytes.make (environment_size operand_size) '\255' in
  (* Write environment fields in little-endian order; the initial 0xff bytes
     preserve the reserved fields in the selected environment format. *)
  let put offset width value =
    for index = 0 to width - 1 do
      Bytes.set bytes (offset + index)
        (Char.of_int_exn
           (Stdlib.Int32.to_int
              (Stdlib.Int32.logand
                 (Stdlib.Int32.shift_right_logical value (8 * index))
                 255l)))
    done
  in
  let word offset value = put offset 2 (Stdlib.Int32.of_int value) in
  if wide then (
    word 0 t.control;
    word 4 t.status;
    word 8 t.tag;
    put 12 4 t.instruction;
    word 16 t.code_segment;
    word 18 t.opcode;
    put 20 4 t.data;
    word 24 t.data_segment)
  else (
    word 0 t.control;
    word 2 t.status;
    word 4 t.tag;
    put 6 2 t.instruction;
    word 8 t.code_segment;
    put 10 2 t.data;
    word 12 t.data_segment);
  Bytes.to_string bytes

(* Restore the x87 environment from its serialized form. *)
let load_environment t ~operand_size bytes =
  if String.length bytes <> environment_size operand_size then
    invalid_arg "Fpu.load_environment: size";
  (* Read fields using the same little-endian layout used by environment. *)
  let get offset width =
    let value = ref 0l in
    for index = 0 to width - 1 do
      value :=
        Stdlib.Int32.logor !value
          (Stdlib.Int32.shift_left
             (Stdlib.Int32.of_int (Char.to_int bytes.[offset + index]))
             (8 * index))
    done;
    !value
  in
  let word offset = Stdlib.Int32.to_int (get offset 2) in
  if Poly.equal operand_size Width.Word then (
    t.control <- word 0;
    t.status <- word 2;
    t.tag <- word 4;
    t.instruction <- get 6 2;
    t.code_segment <- word 8;
    t.data <- get 10 2;
    t.data_segment <- word 12;
    t.opcode <- 0)
  else (
    t.control <- word 0;
    t.status <- word 4;
    t.tag <- word 8;
    t.instruction <- get 12 4;
    t.code_segment <- word 16;
    t.opcode <- word 18 land 0x7ff;
    t.data <- get 20 4;
    t.data_segment <- word 24)

(* Serialize the x87 environment and register stack. *)
let save t ~operand_size =
  let top = (t.status lsr 11) land 7 in
  environment t ~operand_size
  ^ String.concat
      (List.init 8 ~f:(fun index -> t.registers.((top + index) land 7)))

(* Restore the x87 environment and register stack. *)
let restore t ~operand_size bytes =
  let size = environment_size operand_size in
  if String.length bytes <> size + 80 then invalid_arg "Fpu.restore: size";
  load_environment t ~operand_size (String.sub bytes ~pos:0 ~len:size);
  let top = (t.status lsr 11) land 7 in
  for index = 0 to 7 do
    t.registers.((top + index) land 7) <-
      String.sub bytes ~pos:(size + (index * 10)) ~len:10
  done

(* Map a logical stack offset to a physical x87 register index. *)
let index t offset = (((t.status lsr 11) land 7) + offset) land 7

(* Read a nonempty x87 stack register. *)
let get t offset =
  let index = index t offset in
  if (t.tag lsr (index * 2)) land 3 = 3 then
    raise (Extended.Unsupported "x87 stack underflow");
  t.registers.(index)

(* Write an x87 stack register and update its tag. *)
let set t offset value =
  let index = index t offset in
  t.registers.(index) <- value;
  let exponent =
    Char.to_int value.[8] lor ((Char.to_int value.[9] land 127) lsl 8)
  in
  let zero =
    exponent = 0
    && String.for_all (String.sub value ~pos:0 ~len:8) ~f:(Char.equal '\000')
  in
  let tag =
    if zero then 1
    else if
      exponent = 0 || exponent = 0x7fff || Char.to_int value.[7] land 128 = 0
    then 2
    else 0
  in
  t.tag <- t.tag land lnot (3 lsl (index * 2)) lor (tag lsl (index * 2))

(* Push a value onto the x87 register stack. *)
let push t value =
  let top = index t 7 in
  if (t.tag lsr (top * 2)) land 3 <> 3 then
    raise (Extended.Unsupported "x87 stack overflow");
  t.status <- t.status land lnot 0x3800 lor (top lsl 11);
  set t 0 value

(* Mark the top register empty and advance the x87 stack. *)
let pop t =
  let top = index t 0 in
  t.tag <- t.tag lor (3 lsl (top * 2));
  t.status <- t.status land lnot 0x3800 lor (((top + 1) land 7) lsl 11)

(* Compare the stack top with a value and update condition codes. *)
let compare t value =
  let order = Extended.compare (get t 0) value in
  t.status <-
    (t.status land lnot 0x4700
    lor if order < 0 then 0x100 else if order = 0 then 0x4000 else 0)

(* FPREM updates ST(0) and C0/C1/C2/C3 without popping either stack operand. *)
let remainder t =
  let result = Extended.remainder (get t 0) (get t 1) in
  set t 0 result.value;
  let conditions =
    if result.complete then
      ((result.quotient_bits land 4) lsl 6)
      lor ((result.quotient_bits land 2) lsl 13)
      lor ((result.quotient_bits land 1) lsl 9)
    else 0x400
  in
  t.status <- t.status land lnot 0x4700 lor conditions

(* Record metadata for the most recent x87 instruction and data operand. *)
let record t ~instruction ~code_segment ~opcode ~data =
  t.instruction <- instruction;
  t.code_segment <- code_segment;
  t.opcode <- opcode;
  Option.iter data ~f:(fun (address, segment) ->
      t.data <- address;
      t.data_segment <- segment)

(* Mark an x87 stack register as empty. *)
let free t offset = t.tag <- t.tag lor (3 lsl (index t offset * 2))
