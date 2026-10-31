(* Arithmetic flags: CF, PF, AF, ZF, SF, OF; reserved EFLAGS bit 1 is set. *)

(* Logical operations clear CF/OF. Architecturally undefined AF is cleared. *)

open Base
module W = Stdlib.Int32
module U = Stdlib.Int64

type result =
  { value : int32
  ; flags : int32
  }

let unsigned x = U.logand (U.of_int32 x) 0xffff_ffffL
let negative width x = W.compare (Width.sign_extend width x) 0l < 0

let flags ~width ~left ~right ~value ~carry ~overflow =
  let low = W.to_int (W.logand value 255l) in
  (* 0x9669 encodes even parity for each four-bit value. *)
  let nibble = low lxor (low lsr 4) land 15 in
  let parity = (0x9669 lsr nibble) land 1 <> 0 in
  let auxiliary =
    not (W.equal (W.logand (W.logxor (W.logxor left right) value) 0x10l) 0l)
  in
  let bit enabled mask = if enabled then mask else 0 in
  W.of_int
    (2
     lor bit carry 1
     lor bit parity 4
     lor bit auxiliary 16
     lor bit (W.equal value 0l) 64
     lor bit (negative width value) 128
     lor bit overflow 2048)
;;

let add ?(width = Width.Dword) ?(carry = false) left right =
  let left = Width.truncate width left in
  let right = Width.truncate width right in
  let wide = U.add (U.add (unsigned left) (unsigned right)) (if carry then 1L else 0L) in
  let value = Width.truncate width (U.to_int32 wide) in
  let carry = U.compare wide (unsigned (Width.mask width)) > 0 in
  let overflow =
    Bool.equal (negative width left) (negative width right)
    && not (Bool.equal (negative width left) (negative width value))
  in
  { value; flags = flags ~width ~left ~right ~value ~carry ~overflow }
;;

let sub ?(width = Width.Dword) ?(carry = false) left right =
  let left = Width.truncate width left in
  let right = Width.truncate width right in
  let subtrahend = U.add (unsigned right) (if carry then 1L else 0L) in
  let value = Width.truncate width (U.to_int32 (U.sub (unsigned left) subtrahend)) in
  let carry = U.compare (unsigned left) subtrahend < 0 in
  let overflow =
    (not (Bool.equal (negative width left) (negative width right)))
    && not (Bool.equal (negative width left) (negative width value))
  in
  { value; flags = flags ~width ~left ~right ~value ~carry ~overflow }
;;

let logical operation ?(width = Width.Dword) left right =
  let value = Width.truncate width (operation left right) in
  let flags = flags ~width ~left:0l ~right:0l ~value ~carry:false ~overflow:false in
  { value; flags = W.logand flags (W.lognot 16l) }
;;

let logand ?width left right = logical W.logand ?width left right
let logor ?width left right = logical W.logor ?width left right
let logxor ?width left right = logical W.logxor ?width left right

(* PF/ZF/SF for an already width-truncated value. *)
let status_flags ~width value =
  let low = W.to_int (W.logand value 255l) in
  (* 0x9669 encodes even parity for each four-bit value. *)
  let nibble = low lxor (low lsr 4) land 15 in
  let parity = (0x9669 lsr nibble) land 1 <> 0 in
  W.of_int
    ((if parity then 4 else 0)
     lor (if W.equal value 0l then 64 else 0)
     lor if negative width value then 128 else 0)
;;
