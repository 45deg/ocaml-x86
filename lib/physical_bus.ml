(* Physical RAM plus an unclaimed-bus policy. Strict mode is useful for CPU
    unit tests; PC mode returns all ones for unclaimed reads and drops writes.
    No virtual-to-physical aliasing is performed. *)

open Base
module W = Stdlib.Int32
module U = Stdlib.Int64

type t =
  { ram : Memory.t
  ; mutable pc_mode : bool
  }

let create ram = { ram; pc_mode = false }
let enable_pc_mode t = t.pc_mode <- true

let in_ram t address bytes =
  U.compare
    (U.logand (U.of_int32 address) 0xffffffffL)
    (U.of_int (Memory.size t.ram - bytes))
  <= 0
;;

let watch_page_table t address =
  if in_ram t address 1 then Memory.watch_page_table t.ram ~address
;;

let translation_epoch t = Memory.translation_epoch t.ram

let read_ram t width address =
  match width with
  | Width.Byte -> W.of_int (Memory.read_u8 t.ram ~address)
  | Word -> W.of_int (Memory.read_u16 t.ram ~address)
  | Dword -> Memory.read_u32 t.ram ~address
;;

let write_ram t width address value =
  match width with
  | Width.Byte -> Memory.write_u8 t.ram ~address (W.to_int (W.logand value 255l))
  | Word -> Memory.write_u16 t.ram ~address (W.to_int (W.logand value 65535l))
  | Dword -> Memory.write_u32 t.ram ~address value
;;

let read t ~width address =
  if (not t.pc_mode) || in_ram t address (Width.bytes width)
  then read_ram t width address
  else (
    let value = ref 0l in
    for offset = 0 to Width.bytes width - 1 do
      let address = W.add address (W.of_int offset) in
      let byte = if in_ram t address 1 then read_ram t Byte address else 255l in
      value := W.logor !value (W.shift_left byte (offset * 8))
    done;
    !value)
;;

let write t ~width address value =
  if (not t.pc_mode) || in_ram t address (Width.bytes width)
  then write_ram t width address value
  else
    for offset = 0 to Width.bytes width - 1 do
      let address = W.add address (W.of_int offset) in
      if in_ram t address 1
      then write_ram t Byte address (W.shift_right_logical value (offset * 8))
    done
;;

let check t ~address ~width =
  if not t.pc_mode then Memory.check_access t.ram ~address ~width
;;
