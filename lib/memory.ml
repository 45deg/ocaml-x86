(* Zero-initialized physical RAM. Addresses are unsigned 32-bit values.
   Invalid accesses raise [Access_fault] before modifying memory. *)

open Base
module Wide = Stdlib.Int64
module B = Stdlib.Bytes

type t =
  { data : bytes
  ; page_tables : bool array
  ; mutable translation_epoch : int
  }

exception
  Access_fault of
    { address : int32
    ; width : int
    }

let size t = B.length t.data

let create ~size =
  if size <= 0 then invalid_arg "Memory.create: size must be positive";
  { data = B.make size '\000'
  ; page_tables = Array.create ~len:((size + 4095) / 4096) false
  ; translation_epoch = 0
  }
;;

let index t ~address ~width =
  let unsigned = Wide.logand (Wide.of_int32 address) 0xffff_ffffL in
  if width < 0 || Wide.compare unsigned (Wide.of_int (size t - width)) > 0
  then raise (Access_fault { address; width });
  Wide.to_int unsigned
;;

let watch_page_table t ~address =
  let start = index t ~address ~width:1 in
  t.page_tables.(start lsr 12) <- true
;;

let translation_epoch t = t.translation_epoch

(* Any physical alias of a watched page table must invalidate translations,
   including guest clears of accessed/dirty bits and host-side [load]. *)
let changed t start width =
  if width > 0
  then (
    let rec watched page last =
      t.page_tables.(page) || (page < last && watched (page + 1) last)
    in
    if watched (start lsr 12) ((start + width - 1) lsr 12)
    then t.translation_epoch <- t.translation_epoch + 1)
;;

let read_u8 t ~address = B.get_uint8 t.data (index t ~address ~width:1)
let read_u16 t ~address = B.get_uint16_le t.data (index t ~address ~width:2)
let read_u32 t ~address = B.get_int32_le t.data (index t ~address ~width:4)

let write_u8 t ~address value =
  if value < 0 || value > 255 then invalid_arg "Memory.write_u8: not a byte";
  let start = index t ~address ~width:1 in
  changed t start 1;
  B.set_uint8 t.data start value
;;

let write_u16 t ~address value =
  if value < 0 || value > 65535 then invalid_arg "Memory.write_u16: not a word";
  let start = index t ~address ~width:2 in
  changed t start 2;
  B.set_uint16_le t.data start value
;;

let write_u32 t ~address value =
  let start = index t ~address ~width:4 in
  changed t start 4;
  B.set_int32_le t.data start value
;;

let load t ~address bytes =
  let width = String.length bytes in
  let start = index t ~address ~width in
  changed t start width;
  B.blit_string bytes 0 t.data start width
;;

let check_access t ~address ~width = ignore (index t ~address ~width : int)
