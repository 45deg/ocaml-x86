(* Loads a little-endian ELF32 i386 executable at its physical PT_LOAD
    addresses. This is an optional development path for the same Linux kernel,
    after Linux_boot.load has prepared boot_params and the initramfs. *)

open Base
module U = Stdlib.Int64

(* Raise an invalid boot image error with the given reason. *)
let invalid message = raise (Linux_boot.Invalid_image message)

(* Validate and load an ELF32 kernel into physical memory. *)
let load ~memory ~image ~initrd_start =
  if String.length image < 52 then invalid "Truncated ELF header";
  let byte offset = Char.to_int image.[offset] in
  let u16 offset = byte offset lor (byte (offset + 1) lsl 8) in
  let u32 offset =
    let value = ref 0L in
    for index = 0 to 3 do
      value :=
        U.logor !value
          (U.shift_left (U.of_int (byte (offset + index))) (index * 8))
    done;
    U.to_int !value
  in
  if
    (not (String.equal (String.sub image ~pos:0 ~len:4) "\x7fELF"))
    || byte 4 <> 1
    || byte 5 <> 1
    || u16 16 <> 2
    || u16 18 <> 3
  then invalid "Expected ELF32 little-endian i386 executable";
  let headers = u32 28 in
  let stride = u16 42 in
  let count = u16 44 in
  if
    stride < 32
    || headers > String.length image
    || count * stride > String.length image - headers
  then invalid "Truncated ELF program headers";
  let limit =
    if Int32.equal initrd_start 0l then Memory.size memory
    else Int32.to_int_exn initrd_start
  in
  let segments =
    List.filter_map (List.range 0 count) ~f:(fun index ->
        let header = headers + (index * stride) in
        if u32 header <> 1 then None
        else
          let offset = u32 (header + 4) in
          let virtual_address = u32 (header + 8) in
          let address = u32 (header + 12) in
          let filesz = u32 (header + 16) in
          let memsz = u32 (header + 20) in
          if
            filesz > memsz
            || offset > String.length image
            || filesz > String.length image - offset
          then invalid "Truncated ELF load segment";
          if address < 0x100000 || address > limit || memsz > limit - address
          then invalid "ELF segment overlaps reserved memory or initramfs";
          Some (address, virtual_address, offset, filesz, memsz))
  in
  if List.is_empty segments then invalid "ELF has no load segments";
  List.iteri segments ~f:(fun index (address, _, _, _, memsz) ->
      List.iter
        (List.drop segments (index + 1))
        ~f:(fun (other, _, _, _, size) ->
          if address < other + size && other < address + memsz then
            invalid "Overlapping ELF segments"));
  let entry = u32 24 in
  let physical_entry =
    List.find_map segments ~f:(fun (address, virtual_address, _, filesz, _) ->
        if entry >= address && entry - address < filesz then Some entry
        else if entry >= virtual_address && entry - virtual_address < filesz
        then Some (address + entry - virtual_address)
        else None)
  in
  let entry =
    match physical_entry with
    | Some value -> value
    | None -> invalid "ELF entry outside loadable code"
  in
  List.iter segments ~f:(fun (address, _, offset, filesz, memsz) ->
      Memory.load memory
        ~address:(Stdlib.Int32.of_int address)
        (String.sub image ~pos:offset ~len:filesz);
      Memory.load memory
        ~address:(Stdlib.Int32.of_int (address + filesz))
        (String.make (memsz - filesz) '\000'));
  Stdlib.Int32.of_int entry
